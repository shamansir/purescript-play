module Demo.Constructor where

import Prelude

import Effect (Effect)


import Data.Array ((:))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number as Number
import Data.Tuple.Nested ((/\), type (/\))

import Halogen as H
import Halogen.Aff (runHalogenAff, awaitBody) as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as HA
import Halogen.Svg.Elements as HS
import Halogen.VDom.Driver (runUI)

import Yoga.Tree.Extended (children, flatten) as Tree
import Yoga.Tree.Extended.Path as Tree.Path

import Play (Play, (~*))
import Play as Play
import Play.Types (Def, Direction(..), Sizing(..), WithRect) as PT

-- import Test.Demo (renderOne) as Demo
import Test.Demo.Examples (Item(..), ic, itemName, nameOf, noodleUI, playOf, theExamples)

import Test.Demo.Constructor.ColorExtra (colorToText, textToColor)
import Test.Demo.Constructor.ToCode (toCode)
import Test.Demo.Constructor.Play.Extra


main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

-- Path to an item in the tree (array of child indices)
data Field
    = PaddingTop String
    | PaddingLeft String
    | PaddingBottom String
    | PaddingRight String
    | ChildGap String
    | Direction PT.Direction
    | WidthSizing PT.Sizing
    | HeightSizing PT.Sizing


data Action
    = Skip
    | SelectItem ItemPath
    | GoToRoot
    | UpdateName String
    | UpdateColor HA.Color
    | UpdateField Field
    | AddChild Int String
    | RemoveChild Int
    | UpdateChildName String
    | SelectExample Int
    | ToggleCodePanel


type EditingState =
    { name :: String
    , def :: PT.Def
    , color :: Maybe HA.Color
    , fixed ::
        { width :: Maybe Number
        , height :: Maybe Number
        }
    , childName :: String
    }


type State =
    { playTree :: Play Item
    , exampleName :: Maybe String
    , selectedPath :: ItemPath
    , editing :: EditingState
    , codePanelExpanded :: Boolean
    }


-- Helper functions for default values
defaultColor = HA.RGB 128 128 128 :: HA.Color


defaultSizeValue = 100.0 :: Number


component ∷ ∀ (output ∷ Type) (m ∷ Type -> Type) (query ∷ Type -> Type) (t ∷ Type). H.Component query t output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }
    where
        updateSelectedName newName = do
            state <- H.get
            let updatedTree = updateItemAtPath state.selectedPath (setItemName newName) state.playTree
            H.modify_ \s -> s { playTree = updatedTree, editing = s.editing { name = newName } }

        updateSelectedColor color = do
            state <- H.get
            let updatedTree = updateItemAtPath state.selectedPath (setItemColor color) state.playTree
            H.modify_ \s -> s { playTree = updatedTree, editing = s.editing { color = Just color } }

        updateSelectedDef modifyDef = do
            state <- H.get
            let updatedTree = updateDefAtPath state.selectedPath modifyDef state.playTree
            H.modify_ \s -> s { playTree = updatedTree, editing = s.editing { def = modifyDef s.editing.def } }

        updateFixed fn =
            \s -> s { editing = s.editing { fixed = fn s.editing.fixed } }

        loadEditState path tree =
            let
                mbItem = getItemAtPath path tree
                mbDef = getDefAtPath path tree
                mbColor = mbItem >>= \(Item mbCol _) -> mbCol
            in
                { name : fromMaybe "?" (itemName <$> mbItem)
                , def : fromMaybe Play.default mbDef
                , color : mbColor
                , fixed :
                    { width  : isFixedSizing =<< _.width  <$> _.sizing <$> mbDef
                    , height : isFixedSizing =<< _.height <$> _.sizing <$> mbDef
                    }
                , childName: ""
                }

        initialState _ =
            let
                tree = playOf noodleUI
            in
                { playTree: tree
                , selectedPath: []
                , editing: loadEditState [] tree
                , exampleName : Just $ nameOf noodleUI
                , codePanelExpanded: false
                }

        render :: State -> _
        render state =
            HH.div
                [ HP.style "font-family: 'TeX Gyre Adventor', 'JetBrains Sans', Monaco, Helvetica, sans-serif; font-weight: 600; display: flex; gap: 20px;" ]
                [ HH.div
                    [ HP.style "flex: 1; padding: 10px;" ]
                    [ renderExampleSelector state
                    , renderInteractivePreview state
                    ]
                , HH.div
                    [ HP.style "flex: 1; position: relative;" ]
                    [ renderPropertyEditor state
                    , renderCodePanel state
                    ]
                ]

        handleAction = case _ of
            Skip -> pure unit

            SelectItem path -> do
                state <- H.get
                H.modify_ _
                    { selectedPath = path
                    , editing = loadEditState path state.playTree
                    }

            GoToRoot -> H.modify_ _ { selectedPath = [] }

            UpdateName newName -> updateSelectedName newName

            UpdateColor color -> updateSelectedColor color

            UpdateField (PaddingTop str) -> case Number.fromString str of
                Just n -> updateSelectedDef \def -> def { padding = def.padding { top = n } }
                Nothing -> pure unit

            UpdateField (PaddingLeft str) -> case Number.fromString str of
                Just n -> updateSelectedDef \def -> def { padding = def.padding { left = n } }
                Nothing -> pure unit

            UpdateField (PaddingBottom str) -> case Number.fromString str of
                Just n -> updateSelectedDef \def -> def { padding = def.padding { bottom = n } }
                Nothing -> pure unit

            UpdateField (PaddingRight str) -> case Number.fromString str of
                Just n -> updateSelectedDef \def -> def { padding = def.padding { right = n } }
                Nothing -> pure unit

            UpdateField (ChildGap str) -> case Number.fromString str of
                Just n -> updateSelectedDef $ _ { childGap = n }
                Nothing -> pure unit

            UpdateField (Direction dir) -> updateSelectedDef $ _ { direction = dir }

            UpdateField (WidthSizing sizing) -> do
                updateSelectedDef \def -> def { sizing = def.sizing { width = sizing } }
                case sizing of
                    PT.Fixed n ->
                        H.modify_ $ updateFixed $ _ { width = Just n }
                    _ -> pure unit

            UpdateField (HeightSizing sizing) -> do
                updateSelectedDef \def -> def { sizing = def.sizing { height = sizing } }
                case sizing of
                    PT.Fixed n ->
                        H.modify_ $ updateFixed $ _ { height = Just n }
                    _ -> pure unit

            AddChild _ childName -> do
                state <- H.get
                let newChild =
                        Play.i (ic defaultColor childName)
                            ~* Play.width defaultSizeValue
                            ~* Play.height defaultSizeValue
                    updatedTree = addChildAtPath state.selectedPath newChild state.playTree
                H.modify_ _ { playTree = updatedTree }

            RemoveChild childIndex -> do
                state <- H.get
                let updatedTree = removeChildAtPath state.selectedPath childIndex state.playTree
                H.modify_ _ { playTree = updatedTree }

            UpdateChildName newName ->
                H.modify_ \s -> s { editing = s.editing { childName = newName } }

            SelectExample exampleIndex -> do
                case Array.index theExamples exampleIndex of
                    Just example -> do
                        let tree = playOf example
                        H.modify_ _
                            { playTree = tree
                            , selectedPath = []
                            , editing = loadEditState [] tree
                            , exampleName = Just $ nameOf example
                            }
                    Nothing -> pure unit

            ToggleCodePanel ->
                H.modify_ \s -> s { codePanelExpanded = not s.codePanelExpanded }


-- Set item name
setItemName :: String -> Item -> Item
setItemName newName (Item mbCol _) = Item mbCol newName

-- Set item color
setItemColor :: HA.Color -> Item -> Item
setItemColor newColor (Item _ name) = Item (Just newColor) name


isFixedSizing :: PT.Sizing -> Maybe Number
isFixedSizing = case _ of
    PT.Fixed n -> Just n
    _ -> Nothing


-- Render property editor
renderPropertyEditor :: forall i. State -> HH.HTML i Action
renderPropertyEditor state =
    let
        mbCurrentTree = getSubtreeAtPath state.selectedPath state.playTree
        children = fromMaybe [] $ Tree.children <$> Play.toTree <$> mbCurrentTree
        childrenCount = Array.length children
        childName idx =
            let mbName = itemName <$> getItemAtPath (Array.snoc state.selectedPath idx) state.playTree
                mbNonEmptyName = mbName >>= \n -> if n == "" then Nothing else Just n
            in show idx <> ". " <> (fromMaybe "<?>" mbNonEmptyName)
        nextChildName =
            if state.editing.childName /= ""
                then state.editing.childName
                else "New Child (" <> show childrenCount <> ")"
        propertySmallInput ptype name action curValue =
            HH.input
                [ HP.type_ ptype
                , HP.value curValue
                , HE.onValueInput action
                , HP.placeholder name
                , HP.style  "padding: 5px;"
                ]
        propertyFullWidthInput ptype name action curValue =
            HH.div
                [ HP.style "margin-bottom: 15px;" ]
                [ HH.label_ [ HH.text name ]
                , HH.input
                    [ HP.type_ ptype
                    , HP.value curValue
                    , HE.onValueInput action
                    , HP.placeholder name
                    , HP.style "width: 100%; padding: 5px; margin-top: 5px;"
                    ]
                ]
    in
    HH.div
        [ HP.style "padding: 15px; border: 1px solid #ccc; background: #f9f9f9;" ]
        [ HH.h3_ [ HH.text "Property Editor" ]
        , HH.div
            [ HP.style "margin-bottom: 10px;" ]
            [ HH.button
                [ HE.onClick \_ -> GoToRoot
                , HP.style "padding: 5px 10px; background: #007bff; color: white; border: none; border-radius: 3px; cursor: pointer;"
                ]
                [ HH.text "← Root" ]
            , HH.button
                [ HE.onClick \_ -> SelectItem $ Array.dropEnd 1 state.selectedPath
                , HP.style "margin-left: 10px; padding: 5px 10px; background: #c9ae4bff; color: white; border: none; border-radius: 3px; cursor: pointer;"
                ]
                [ HH.text "← Parent" ]
            , HH.span
                [ HP.style "margin-left: 10px; color: #666;" ]
                [ HH.text $ "Path: " <> show state.selectedPath ]
            ]
        , propertyFullWidthInput HP.InputText "Item Name" UpdateName state.editing.name
        , HH.div
            [ HP.style "margin-bottom: 15px;" ]
            [ HH.label_ [ HH.text "Color:" ]
            , renderColorSelect state.editing.color
            ]
        , HH.div
            [ HP.style "margin-bottom: 15px;" ]
            [ HH.label_ [ HH.text "Direction:" ]
            , HH.select
                [ HE.onSelectedIndexChange \idx -> case idx of
                    0 -> UpdateField $ Direction PT.LeftToRight
                    1 -> UpdateField $ Direction PT.TopToBottom
                    _ -> UpdateField $ Direction PT.LeftToRight
                , HP.style "width: 100%; padding: 5px; margin-top: 5px;"
                ]
                [ HH.option [ HP.selected (state.editing.def.direction == PT.LeftToRight) ] [ HH.text "Left to Right" ]
                , HH.option [ HP.selected (state.editing.def.direction == PT.TopToBottom) ] [ HH.text "Top to Bottom" ]
                ]
            ]
        , HH.div
            [ HP.style "margin-bottom: 15px;" ]
            [ HH.label_ [ HH.text "Width Sizing:" ]
            , renderSizingSelect
                state.editing
                (_.fixed >>> _.width)
                state.editing.def.sizing.width
                $ UpdateField <<< WidthSizing
            ]
        , HH.div
            [ HP.style "margin-bottom: 15px;" ]
            [ HH.label_ [ HH.text "Height Sizing:" ]
            , renderSizingSelect
                state.editing
                (_.fixed >>> _.height)
                state.editing.def.sizing.height
                $ UpdateField <<< HeightSizing
            ]
        , HH.div
            [ HP.style "margin-bottom: 15px;" ]
            [ HH.label_ [ HH.text "Padding:" ]
            , HH.div
                [ HP.style "display: grid; grid-template-columns: 1fr 1fr; gap: 5px; margin-top: 5px;" ]
                [ propertySmallInput HP.InputNumber "Top" (UpdateField <<< PaddingTop) (show state.editing.def.padding.top)
                , propertySmallInput HP.InputNumber "Right" (UpdateField <<< PaddingRight) (show state.editing.def.padding.right)
                , propertySmallInput HP.InputNumber "Bottom" (UpdateField <<< PaddingBottom) (show state.editing.def.padding.bottom)
                , propertySmallInput HP.InputNumber "Left" (UpdateField <<< PaddingLeft) (show state.editing.def.padding.left)
                ]
            ]
        , propertyFullWidthInput HP.InputNumber "Child Gap" (UpdateField <<< ChildGap) $ show state.editing.def.childGap
        , HH.div
            [ HP.style "margin-bottom: 15px;" ]
            [ HH.h4_ [ HH.text $ "Children (" <> show childrenCount <> "):" ]
            , HH.div_
                $ Array.mapWithIndex (\i child ->
                    HH.div
                        [ HP.style "display: flex; align-items: center; gap: 10px; margin: 5px 0;" ]
                        [ HH.span_ [ HH.text $ childName i ]
                        , HH.button
                            [ HE.onClick \_ -> SelectItem $ Array.snoc state.selectedPath i
                            , HP.style "padding: 2px 8px; background: #35ac45; color: white; border: none; border-radius: 3px; cursor: pointer;"
                            ]
                            [ HH.text "Go" ]
                        , HH.button
                            [ HE.onClick \_ -> RemoveChild i
                            , HP.style "padding: 2px 8px; background: #dc3545; color: white; border: none; border-radius: 3px; cursor: pointer;"
                            ]
                            [ HH.text "Remove" ]
                        ]
                ) children
            , HH.div
                [ HP.style "margin-top: 10px;" ]
                [ HH.input
                    [ HP.type_ HP.InputText
                    , HP.placeholder nextChildName
                    , HP.id "new-child-name"
                    , HP.style "padding: 5px; margin-right: 10px;"
                    , HE.onValueInput UpdateChildName
                    ]
                , HH.button
                    [ HE.onClick \_ -> AddChild childrenCount nextChildName
                    , HP.style "padding: 5px 10px; background: #28a745; color: white; border: none; border-radius: 3px; cursor: pointer;"
                    ]
                    [ HH.text "Add Child" ]
                ]
            ]
        ]

renderSizingSelect :: forall i. EditingState -> (EditingState -> Maybe Number) -> PT.Sizing -> (PT.Sizing -> Action) -> HH.HTML i Action
renderSizingSelect editingState fromEditingState currentSizing updateAction =
    let
        mbEditingValue = fromEditingState editingState
        isInputEnabled = isJust $ isFixedSizing currentSizing
        isInputDisabled = not isInputEnabled
        isSelectBoxEnabled = not isInputEnabled
        isSelectBoxDisabled = not isSelectBoxEnabled
    in
    HH.div_
        [ HH.div
            [ HP.style "display: flex; align-items: center; gap: 10px; margin-top: 5px;" ]
            [ HH.input
                [ HP.type_ HP.InputCheckbox
                , HP.checked isInputEnabled
                , HE.onChecked \checked ->
                    if checked
                    then updateAction $ PT.Fixed $ fromMaybe defaultSizeValue mbEditingValue
                    else updateAction $ PT.Fit
                , HP.id "fixed-checkbox"
                ]
            , HH.label
                [ HP.for "fixed-checkbox"
                , HP.style "margin: 0;"
                ]
                [ HH.text "Fixed size:" ]
            , HH.input
                [ HP.type_ HP.InputNumber
                , HP.value $ show $ fromMaybe defaultSizeValue mbEditingValue
                , HE.onValueInput \str -> case Number.fromString str of
                    Just n -> updateAction $ PT.Fixed n
                    Nothing -> Skip
                , HP.disabled isInputDisabled
                , HP.style $ "padding: 5px; width: 80px;" <>
                    if isInputDisabled then " opacity: 0.5;" else ""
                , HP.placeholder "---"
                ]
            ]
        , HH.select
            [ HE.onSelectedIndexChange \idx -> case idx of
                0 -> updateAction PT.None
                1 -> updateAction PT.Fit
                2 -> updateAction PT.Grow
                3 -> updateAction PT.FitGrow
                _ -> updateAction PT.None
            , HP.disabled isSelectBoxDisabled
            , HP.style $ "width: 100%; padding: 5px; margin-top: 5px;" <>
                if isSelectBoxDisabled then " opacity: 0.5;" else ""
            ]
            [ HH.option [ HP.selected (currentSizing == PT.None) ] [ HH.text "None" ]
            , HH.option [ HP.selected (currentSizing == PT.Fit) ] [ HH.text "Fit" ]
            , HH.option [ HP.selected (currentSizing == PT.Grow) ] [ HH.text "Grow" ]
            , HH.option [ HP.selected (currentSizing == PT.FitGrow) ] [ HH.text "FitGrow" ]
            ]
        ]

renderColorSelect :: forall i. Maybe HA.Color -> HH.HTML i Action
renderColorSelect currentColor =
    let
        currentText = case currentColor of
            Just color -> colorToText color
            Nothing -> "transparent"
    in
    HH.input
        [ HP.type_ HP.InputText
        , HP.value currentText
        , HE.onValueInput $ maybe Skip UpdateColor <<< textToColor
        , HP.placeholder "Named color or #hex (e.g., red, #ff0000)"
        , HP.style "width: 100%; padding: 5px; margin-top: 5px; font-family: monospace;"
        ]

renderCodePanel :: forall i. State -> HH.HTML i Action
renderCodePanel state =
    let
        codeContent = fromMaybe "-" $ toCode (itemName >>> show) <$> getSubtreeAtPath state.selectedPath state.playTree
        arrowSymbol = if state.codePanelExpanded then "▼" else "▶"

        collapsedStyle = "position: fixed; bottom: 0; left: 0; right: 0; z-index: 1000; background: #f0f0f0; border-top: 2px solid #ccc; cursor: pointer;"
        expandedPanelStyle = "position: fixed; bottom: 0; left: 20%; right: 60%; height: 40vh; z-index: 1000; background: #f0f0f0; border: 2px solid #ccc; border-radius: 8px 8px 0 0; box-shadow: 0 -4px 12px rgba(0,0,0,0.15);"

        titleStyle = "padding: 8px 15px; font-weight: bold; border-bottom: 1px solid #ccc; cursor: pointer; user-select: none; display: flex; align-items: center; gap: 8px;"

        contentStyle = "height: calc(100% - 40px); overflow: auto;"
        isExpanded = state.codePanelExpanded
    in
        HH.div
            [ HP.style $ if isExpanded then expandedPanelStyle else collapsedStyle ]
            $ HH.div
                [ HP.style titleStyle
                , HE.onClick \_ -> ToggleCodePanel
                ]
                [ HH.span_ [ HH.text arrowSymbol ]
                , HH.text "Generated Code"
                ]
            : if isExpanded then
                pure $ HH.div
                    [ HP.style contentStyle ]
                    [ HH.textarea
                        [ HP.value codeContent
                        , HP.style "width: 100%; height: 100%; font-family: 'Courier New', Courier, monospace; font-size: 12px; background: #f0f0f0; border: none; padding: 10px; box-sizing: border-box; resize: none; outline: none;"
                        , HP.readOnly true
                        ]
                    ]
                else []


renderExampleSelector :: forall i. State -> HH.HTML i Action
renderExampleSelector state =
    let
        currentExampleName = fromMaybe "Noodle UI" state.exampleName
    in
    HH.div
        [ HP.style "margin-bottom: 15px;" ]
        [ HH.label_ [ HH.text "Select Example:" ]
        , HH.select
            [ HE.onSelectedIndexChange SelectExample
            , HP.style "width: 100%; padding: 5px; margin-top: 5px;"
            ]
            $ Array.mapWithIndex (\_ example ->
                HH.option
                    [ HP.selected (nameOf example == currentExampleName) ]
                    [ HH.text $ nameOf example ]
            ) theExamples
        ]

-- Render interactive preview
renderInteractivePreview :: forall i. State -> HH.HTML i Action
renderInteractivePreview state =
    let
        layout = Play.layout state.playTree
        layoutTree = Play.layoutToTree layout
        size = Play.layoutSize layout
    in
    HH.div_
        [ HH.h3_ [ HH.text $ fromMaybe "Interactive Preview" state.exampleName ]
        , HS.svg
            [ HA.width size.width
            , HA.height size.height
            , HP.style "border: 1px solid #ccc;"
            ]
            $ renderInteractiveItem state <$>
                (Tree.flatten
                    $ map (lmap Tree.Path.toArray)
                    $ Tree.Path.fill
                    $ layoutTree
                )
        ]

renderInteractiveItem :: forall i. State -> (ItemPath /\ PT.WithRect Item) -> HH.HTML i Action
renderInteractiveItem state (path /\ { v, rect }) =
    case v of
        Item mbCol itemLabel ->
            let isSelected = state.selectedPath == path
            in HS.g
                [ HE.onClick \_ -> SelectItem path
                , HP.style "pointer-events: all; cursor: pointer;"
                ]
                [ HS.rect
                    [ HA.x rect.pos.x
                    , HA.y rect.pos.y
                    , HA.rx 3.0
                    , HA.ry 3.0
                    , HA.width rect.size.width
                    , HA.height rect.size.height
                    , HA.fill $ case mbCol of
                        Just col -> col
                        Nothing -> HA.Named "transparent"
                    , HA.stroke $ if isSelected then HA.RGB 255 0 0 else case mbCol of
                        Just _ -> HA.Named "transparent"
                        Nothing -> HA.Named "black"
                    , HA.strokeWidth $ if isSelected then 2.0 else 0.5
                    , HP.style "cursor: pointer;"
                    , HE.onClick \_ -> SelectItem path
                    ]
                , HS.text
                    [ HA.x $ rect.pos.x + 5.0
                    , HA.y $ rect.pos.y + 7.0
                    , HA.fontSize $ HA.FontSizeLength $ HA.Px 14.0
                    , HA.fill $ HA.Named "white"
                    , HA.strokeWidth 0.5
                    , HA.dominantBaseline HA.Hanging
                    , HP.style "pointer-events: none;"
                    , HE.onClick \_ -> SelectItem path
                    ]
                    [ HH.text itemLabel ]
                ]

