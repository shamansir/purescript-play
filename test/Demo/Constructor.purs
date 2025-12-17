module Demo.Constructor where

import Prelude

import Effect (Effect)

import Data.Array ((:))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (wrap, unwrap)
import Data.Number as Number
import Data.String as String
import Data.Tuple.Nested ((/\), type (/\))

import Halogen as H
import Halogen.Aff (runHalogenAff, awaitBody) as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as HA
import Halogen.Svg.Elements as HS
import Halogen.VDom.Driver (runUI)

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (children, flatten, value) as Tree
import Yoga.Tree.Extended.Path as Tree.Path
import Yoga.JSON (class WriteForeign, writeImpl)

import Play (Play, (~*))
import Play as Play
import Play.Extra as Play
import Play.Types (Def, Direction(..), Sizing(..), WithDef, WithRect, WithDefRect, Percents(..), Align(..), HAlign(..), VAlign(..)) as PT

import Test.Demo as Demo
import Test.Demo.Constructor.ColorExtra (colorToText, textToColor)
import Test.Demo.Constructor.ToCode (toCode, encodeDef) as Play
import Test.Demo.Examples (selectedExamples, ExItem(..))
import Test.Demo.Examples.Noodle.App (noodleUI)
import Test.Demo.Examples.Types (class IsItem, itemColor, itemName, nameOf, playOf, nextItem, class RenderItem, renderItem, class NextItem)


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
    | AlignHorz PT.Align
    | AlignVert PT.Align


data Action
    = Skip
    | SelectItem Play.ItemPath
    | GoToRoot
    | GoToPreviousSibling
    | GoToNextSibling
    | UpdateName String
    | UpdateColor HA.Color
    | UpdateField Field
    | AddChild Int String
    | RemoveChild Int
    | UpdateChildName String
    | SelectExample Int
    | ToggleCodePanel
    | SelectCodeTab Int
    | ToggleNodeCollapsed Play.ItemPath
    | ToggleShowEncodedSizing


data Axis = Horz | Vert
derive instance Eq Axis


type EditingState =
    { name :: String
    , def :: PT.Def
    , color :: Maybe HA.Color
    , sizing ::
        { width :: Maybe PT.Sizing
        , height :: Maybe PT.Sizing
        }
    , childName :: String
    }


type CodePanelState =
    { expanded :: Boolean
    , tabIndex :: Int
    , collapsedNodes :: Array Play.ItemPath
    }


type PropChanges =
    { name :: String
    , mbColor :: Maybe HA.Color
    }


newtype ItemWithChanges x =
    IWC
        { item :: x
        , changes :: PropChanges
        }


type State =
    { playTree :: Play (ItemWithChanges ExItem)
    , exampleName :: Maybe String
    , selectedPath :: Play.ItemPath
    , editing :: EditingState
    , codePanel :: CodePanelState
    , showEncodedSizing :: Boolean
    }


defaultSizeValue = 100.0 :: Number
defaultMinSizeValue = 50.0 :: Number
defaultMaxSizeValue = 150.0 :: Number


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
            let updatedTree = Play.updateAt state.selectedPath (setItemName newName) state.playTree
            H.modify_ \s -> s { playTree = updatedTree, editing = s.editing { name = newName } }

        updateSelectedColor color = do
            state <- H.get
            let updatedTree = Play.updateAt state.selectedPath (setItemColor color) state.playTree
            H.modify_ \s -> s { playTree = updatedTree, editing = s.editing { color = Just color } }

        updateSelectedDef modifyDef = do
            state <- H.get
            let updatedTree = Play.updateDefAt state.selectedPath modifyDef state.playTree
            H.modify_ \s -> s { playTree = updatedTree, editing = s.editing { def = modifyDef s.editing.def } }

        loadEditState :: Play.ItemPath -> Play (ItemWithChanges ExItem) -> EditingState
        loadEditState path tree =
            let
                mbItem = Play.itemAt path tree
                mbDef = Play.defAt path tree
                mbColor = mbItem >>= itemColor
            in
                { name : fromMaybe "?" (itemName <$> mbItem)
                , def : fromMaybe Play.default mbDef
                , color : mbColor
                , sizing :
                    { width  : _.width  <$> _.sizing <$> mbDef
                    , height : _.height <$> _.sizing <$> mbDef
                    }
                , childName: ""
                }

        initialState :: _ -> State
        initialState _ =
            let
                tree = playOf $ initChanges <$> Noodle <$> noodleUI
            in
                { playTree: tree
                , selectedPath: []
                , editing: loadEditState [] tree
                , exampleName : Just $ nameOf noodleUI
                , codePanel: { expanded: false
                             , tabIndex: 0
                             , collapsedNodes: []
                             }
                , showEncodedSizing: false
                }

        render :: State -> _
        render state =
            HH.div
                [ HP.style "font-family: 'TeX Gyre Adventor', 'JetBrains Sans', Monaco, Helvetica, sans-serif; font-weight: 600; display: flex; gap: 20px;" ]
                [ HH.div
                    [ HP.style "flex: 1; padding: 10px;" ]
                    [ renderExampleSelector state
                    , renderClickablePreview state
                    , renderCodePanel state
                    ]
                , HH.div
                    [ HP.style "flex: 1;" ]
                    [ renderPropertyEditor state
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

            GoToRoot ->
                H.modify_ \s ->
                    s
                        { selectedPath = []
                        , editing = loadEditState [] s.playTree
                        }

            GoToPreviousSibling -> do
                state <- H.get
                case Array.unsnoc state.selectedPath of
                    Nothing -> pure unit  -- At root, no siblings
                    Just { init: parentPath, last: currentIndex } ->
                        if currentIndex > 0 then
                            let newPath = Array.snoc parentPath (currentIndex - 1)
                            in H.modify_ _
                                { selectedPath = newPath
                                , editing = loadEditState newPath state.playTree
                                }
                        else pure unit

            GoToNextSibling -> do
                state <- H.get
                case Array.unsnoc state.selectedPath of
                    Nothing -> pure unit  -- At root, no siblings
                    Just { init: parentPath, last: currentIndex } -> do
                        let
                            mbParentTree = Play.playAt parentPath state.playTree
                            siblingCount = fromMaybe 0 $ Array.length <$> Tree.children <$> Play.toTree <$> mbParentTree
                        if currentIndex < siblingCount - 1 then
                            let newPath = Array.snoc parentPath (currentIndex + 1)
                            in H.modify_ _
                                { selectedPath = newPath
                                , editing = loadEditState newPath state.playTree
                                }
                        else pure unit

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
                H.modify_ \s -> s { editing = s.editing { sizing = s.editing.sizing { width = Just sizing } } }

            UpdateField (HeightSizing sizing) -> do
                updateSelectedDef \def -> def { sizing = def.sizing { height = sizing } }
                H.modify_ \s -> s { editing = s.editing { sizing = s.editing.sizing { height = Just sizing } } }

            UpdateField (AlignHorz align) -> updateSelectedDef $ \def -> def { alignment = def.alignment { horizontal = PT.Horz align } }

            UpdateField (AlignVert align) -> updateSelectedDef $ \def -> def { alignment = def.alignment { vertical   = PT.Vert align } }

            AddChild _ childName -> do
                state <- H.get
                let newChild =
                        Play.i (nextItem childName)
                            ~* Play.width defaultSizeValue
                            ~* Play.height defaultSizeValue
                    updatedTree = Play.addChildAt state.selectedPath newChild state.playTree
                H.modify_ _ { playTree = updatedTree }

            RemoveChild childIndex -> do
                state <- H.get
                let updatedTree = Play.removeChildAt state.selectedPath childIndex state.playTree
                H.modify_ _ { playTree = updatedTree }

            UpdateChildName newName ->
                H.modify_ \s -> s { editing = s.editing { childName = newName } }

            SelectExample exampleIndex -> do
                case Array.index selectedExamples exampleIndex of
                    Just example -> do
                        let tree = initChanges <$> playOf example
                        H.modify_ _
                            { playTree = tree
                            , selectedPath = []
                            , editing = loadEditState [] tree
                            , exampleName = Just $ nameOf example
                            }
                    Nothing -> pure unit

            ToggleCodePanel ->
                H.modify_ \s -> s { codePanel = s.codePanel { expanded = not s.codePanel.expanded } }

            SelectCodeTab tabIndex ->
                H.modify_ \s -> s { codePanel = s.codePanel { tabIndex = tabIndex } }

            ToggleNodeCollapsed path -> do
                state <- H.get
                let
                    isCollapsed = Array.elem path state.codePanel.collapsedNodes
                    updatedCollapsed = if isCollapsed
                        then Array.filter (_ /= path) state.codePanel.collapsedNodes
                        else Array.snoc state.codePanel.collapsedNodes path
                H.modify_ \s -> s { codePanel = s.codePanel { collapsedNodes = updatedCollapsed } }

            ToggleShowEncodedSizing ->
                H.modify_ \s -> s { showEncodedSizing = not s.showEncodedSizing }


-- Set item name
setItemName :: forall x. String -> ItemWithChanges x -> ItemWithChanges x
setItemName newName (IWC { item, changes }) = IWC { item, changes : changes { name = newName } }

-- Set item color
setItemColor :: forall x. HA.Color -> ItemWithChanges x -> ItemWithChanges x
setItemColor newColor (IWC { item, changes }) = IWC { item, changes : changes { mbColor = Just newColor } }

isFixedSizing :: PT.Sizing -> Maybe Number
isFixedSizing = case _ of
    PT.Fixed n -> Just n
    _ -> Nothing


-- Render property editor
renderPropertyEditor :: forall i. State -> HH.HTML i Action
renderPropertyEditor state =
    let
        mbCurrentTree = Play.playAt state.selectedPath state.playTree
        children = fromMaybe [] $ Tree.children <$> Play.toTree <$> mbCurrentTree
        childrenCount = Array.length children
        childName idx =
            let mbName = itemName <$> Play.itemAt (Array.snoc state.selectedPath idx) state.playTree
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
                [ HP.style "margin-bottom: 15px; font-size: 0.9em;" ]
                [ HH.label_ [ HH.text name ]
                , HH.input
                    [ HP.type_ ptype
                    , HP.value curValue
                    , HE.onValueInput action
                    , HP.placeholder name
                    , HP.style "width: 100%; padding: 5px; margin-top: 5px;"
                    ]
                ]
        hasPreviousSibling :: State -> Boolean
        hasPreviousSibling st =
            case Array.unsnoc st.selectedPath of
                Nothing -> false
                Just { last: currentIndex } -> currentIndex > 0

        hasNextSibling :: State -> Boolean
        hasNextSibling st =
            case Array.unsnoc st.selectedPath of
                Nothing -> false
                Just { init: parentPath, last: currentIndex } ->
                    let
                        mbParentTree = Play.playAt parentPath st.playTree
                        siblingCount = fromMaybe 0 $ Array.length <$> Tree.children <$> Play.toTree <$> mbParentTree
                    in currentIndex < siblingCount - 1

        rootButtonStyle = "padding: 5px 10px; color: white; border: none; border-radius: 3px; " <> if state.selectedPath == [] then
            "background: #6c757d; cursor: not-allowed; opacity: 0.6;"
        else
            "background: #007bff; cursor: pointer;"

        parentButtonStyle = "padding: 5px 10px; color: white; border: none; border-radius: 3px; " <> if state.selectedPath == [] then
            "background: #6c757d; cursor: not-allowed; opacity: 0.6;"
        else
            "background: #c9ae4bff; cursor: pointer;"

        siblingButtonStyle enabled = "padding: 5px 10px; color: white; border: none; border-radius: 3px; " <> if enabled then
            "background: #17a2b8; cursor: pointer;"
        else
            "background: #6c757d; cursor: not-allowed; opacity: 0.6;"

        -- Helper for rendering alignment radio buttons
        renderAlignmentRadio :: forall j. Axis -> String -> PT.Align -> (PT.Align -> Action) -> HH.HTML j Action
        renderAlignmentRadio axis groupName currentAlign updateAction =
            HH.div
                [ HP.style "display: flex; gap: 10px; align-items: center;" ]
                [ renderAlignOption groupName (if axis == Horz then "Left" else "Top") PT.Start currentAlign updateAction
                , renderAlignOption groupName (if axis == Horz then "Middle" else "Center") PT.Center currentAlign updateAction
                , renderAlignOption groupName (if axis == Horz then "Right" else "Bottom") PT.End currentAlign updateAction
                ]

        renderAlignOption :: forall j. String -> String -> PT.Align -> PT.Align -> (PT.Align -> Action) -> HH.HTML j Action
        renderAlignOption groupName label alignment currentAlign updateAction =
            HH.label
                [ HP.style "display: flex; align-items: center; gap: 3px; cursor: pointer; font-size: 0.85em;" ]
                [ HH.input
                    [ HP.type_ HP.InputRadio
                    , HP.name groupName
                    , HP.checked (currentAlign == alignment)
                    , HE.onChecked \_ -> updateAction alignment
                    , HP.style "margin: 0;"
                    ]
                , HH.text label
                ]
    in
    HH.div
        [ HP.style "padding: 15px; border: 1px solid #ccc; background: #f9f9f9;" ]
        [ {- HH.h3_ [ HH.text "Property Editor" ]
        , -} HH.div
            [ HP.style "margin-bottom: 10px; display: flex; gap: 10px; align-items: center;" ]
            [ HH.button
                [ HE.onClick \_ -> GoToRoot
                , HP.style rootButtonStyle
                , HP.disabled (state.selectedPath == [])
                ]
                [ HH.text "← Root" ]
            , HH.button
                [ HE.onClick \_ -> SelectItem $ Array.dropEnd 1 state.selectedPath
                , HP.style parentButtonStyle
                , HP.disabled (state.selectedPath == [])
                ]
                [ HH.text "← Parent" ]
            , HH.button
                [ HE.onClick \_ -> GoToPreviousSibling
                , HP.style $ siblingButtonStyle $ hasPreviousSibling state
                , HP.disabled (not $ hasPreviousSibling state)
                ]
                [ HH.text "<<" ]
            , HH.button
                [ HE.onClick \_ -> GoToNextSibling
                , HP.style $ siblingButtonStyle $ hasNextSibling state
                , HP.disabled (not $ hasNextSibling state)
                ]
                [ HH.text ">>" ]
            , HH.span
                [ HP.style "color: #666; font-size: 0.9em;" ]
                [ HH.text $ "Path: " <> show state.selectedPath ]
            ]
        , propertyFullWidthInput HP.InputText "Item Name:" UpdateName state.editing.name
        , HH.div
            [ HP.style "margin-bottom: 15px; font-size: 0.9em;" ]
            [ HH.label_ [ HH.text "Color:" ]
            , renderColorSelect state.editing.color
            ]
        , HH.div
            [ HP.style "margin-bottom: 15px; font-size: 0.9em;" ]
            [ HH.label_ [ HH.text "Direction:" ]
            , HH.select
                [ HE.onSelectedIndexChange \idx -> case idx of
                    0 -> UpdateField $ Direction PT.LeftToRight
                    1 -> UpdateField $ Direction PT.TopToBottom
                    2 -> UpdateField $ Direction PT.BackToFront
                    _ -> UpdateField $ Direction PT.LeftToRight
                , HP.style "width: 100%; padding: 5px; margin-top: 5px;"
                ]
                [ HH.option [ HP.selected (state.editing.def.direction == PT.LeftToRight) ] [ HH.text "→ Left to Right" ]
                , HH.option [ HP.selected (state.editing.def.direction == PT.TopToBottom) ] [ HH.text "↓ Top to Bottom" ]
                , HH.option [ HP.selected (state.editing.def.direction == PT.BackToFront) ] [ HH.text "≡ Back to Front" ]
                ]
            ]
        , HH.div
            [ HP.style "margin-bottom: 15px;" ]
            [ HH.div
                [ HP.style "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;" ]
                [ HH.div_
                    [ HH.label [ HP.style "font-size: 0.9em;" ] [ HH.text "Width Sizing:" ]
                    , renderSizingRadio
                        "width-sizing"
                        state.editing
                        (_.sizing >>> _.width)
                        state.editing.def.sizing.width
                        $ UpdateField <<< WidthSizing
                    ]
                , HH.div_
                    [ HH.label [ HP.style "font-size: 0.9em;" ] [ HH.text "Height Sizing:" ]
                    , renderSizingRadio
                        "height-sizing"
                        state.editing
                        (_.sizing >>> _.height)
                        state.editing.def.sizing.height
                        $ UpdateField <<< HeightSizing
                    ]
                ]
            ]
        , HH.div
            [ HP.style "margin-bottom: 15px; font-size: 0.9em;" ]
            [ HH.label_ [ HH.text "Padding:" ]
            , HH.div
                [ HP.style "display: grid; grid-template-columns: 1fr 1fr; gap: 5px; margin-top: 5px;" ]
                [ propertySmallInput HP.InputNumber "Top" (UpdateField <<< PaddingTop) (show state.editing.def.padding.top)
                , propertySmallInput HP.InputNumber "Right" (UpdateField <<< PaddingRight) (show state.editing.def.padding.right)
                , propertySmallInput HP.InputNumber "Bottom" (UpdateField <<< PaddingBottom) (show state.editing.def.padding.bottom)
                , propertySmallInput HP.InputNumber "Left" (UpdateField <<< PaddingLeft) (show state.editing.def.padding.left)
                ]
            ]
        , propertyFullWidthInput HP.InputNumber "Child Gap:" (UpdateField <<< ChildGap) $ show state.editing.def.childGap
        , HH.div
            [ HP.style "margin-bottom: 15px;" ]
            [ HH.label [ HP.style "font-size: 0.9em; display: block; margin-bottom: 4px;" ] [ HH.text "Children Align:" ]
            , HH.div
                [ HP.style "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;" ]
                [ HH.div_
                    [ {- HH.label [ HP.style "font-size: 0.7em; display: block; margin-bottom: 4px;" ] [ HH.text "Horizontal:" ]
                    , -} renderAlignmentRadio Horz "align-h" (unwrap state.editing.def.alignment.horizontal) $ UpdateField <<< AlignHorz
                    ]
                , HH.div_
                    [ {- HH.label [ HP.style "font-size: 0.7em; display: block; margin-bottom: 4px;" ] [ HH.text "Vertical:" ]
                    , -} renderAlignmentRadio Vert "align-v" (unwrap state.editing.def.alignment.vertical) $ UpdateField <<< AlignVert
                    ]
                ]
            ]
        , HH.div
            [ HP.style "margin-bottom: 15px; font-size: 0.9em;" ]
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


renderSizingRadio
    :: forall i
     . String                           -- Unique name for radio group
    -> EditingState                     -- Current editing state
    -> (EditingState -> Maybe PT.Sizing) -- Extract sizing from editing state
    -> PT.Sizing                        -- Current sizing value
    -> (PT.Sizing -> Action)            -- Update action
    -> HH.HTML i Action
renderSizingRadio radioName editingState fromEditingState currentSizing updateAction =
    let
        -- Get the stored sizing from editing state, or use current sizing as fallback
        editingSizing = fromMaybe currentSizing (fromEditingState editingState)

        -- Base style for all radio options
        baseRadioStyle = "margin: 4px 0; display: flex; align-items: center; gap: 5px; min-height: 24px;"

        -- Simple radio option without inputs
        simpleRadioOption label sizing =
            HH.div
                [ HP.style baseRadioStyle ]
                [ HH.input
                    [ HP.type_ HP.InputRadio
                    , HP.name radioName
                    , HP.checked (currentSizing == sizing)
                    , HE.onChecked \_ -> updateAction sizing
                    , HP.id $ radioName <> "-" <> label
                    , HP.style "margin: 0; flex-shrink: 0;"
                    ]
                , HH.label
                    [ HP.for $ radioName <> "-" <> label
                    , HP.style "margin: 0; cursor: pointer; flex-shrink: 0;"
                    ]
                    [ HH.text label ]
                ]

        -- Radio option with one number input
        radioOptionWithInput label isCurrentType getValue createSizing placeholderText =
            let
                isSelected = isCurrentType currentSizing
                currentValue = if isSelected then getValue currentSizing else getValue editingSizing
            in
            HH.div
                [ HP.style baseRadioStyle ]
                [ HH.input
                    [ HP.type_ HP.InputRadio
                    , HP.name radioName
                    , HP.checked isSelected
                    , HE.onChecked \_ -> updateAction $ createSizing currentValue
                    , HP.id $ radioName <> "-" <> label
                    , HP.style "margin: 0; flex-shrink: 0;"
                    ]
                , HH.label
                    [ HP.for $ radioName <> "-" <> label
                    , HP.style "margin: 0; cursor: pointer; flex-shrink: 0; min-width: 80px;"
                    ]
                    [ HH.text $ label <> ":" ]
                , HH.input
                    [ HP.type_ HP.InputNumber
                    , HP.value $ show currentValue
                    , HE.onValueInput \str -> case Number.fromString str of
                        Just n -> updateAction $ createSizing n
                        Nothing -> Skip
                    , HP.disabled (not isSelected)
                    , HP.style $ "padding: 3px 5px; width: 70px; font-size: 0.85em; box-sizing: border-box;" <>
                        if not isSelected then " opacity: 0.5;" else ""
                    , HP.placeholder placeholderText
                    ]
                ]

        -- Radio option with two number inputs (min/max)
        radioOptionWithMinMax label isCurrentType getValues createSizing =
            let
                isSelected = isCurrentType currentSizing
                currentValues = if isSelected then getValues currentSizing else getValues editingSizing
            in
            HH.div
                [ HP.style baseRadioStyle ]
                [ HH.input
                    [ HP.type_ HP.InputRadio
                    , HP.name radioName
                    , HP.checked isSelected
                    , HE.onChecked \_ -> updateAction $ createSizing currentValues
                    , HP.id $ radioName <> "-" <> label
                    , HP.style "margin: 0; flex-shrink: 0;"
                    ]
                , HH.label
                    [ HP.for $ radioName <> "-" <> label
                    , HP.style "margin: 0; cursor: pointer; white-space: nowrap; flex-shrink: 0; min-width: 80px;"
                    ]
                    [ HH.text $ label <> ":" ]
                , HH.div
                    [ HP.style "display: flex; align-items: center; gap: 3px;" ]
                    [ HH.input
                        [ HP.type_ HP.InputNumber
                        , HP.value $ show currentValues.min
                        , HE.onValueInput \str -> case Number.fromString str of
                            Just n -> updateAction $ createSizing { min: n, max: currentValues.max }
                            Nothing -> Skip
                        , HP.disabled (not isSelected)
                        , HP.style $ "padding: 3px 5px; width: 55px; font-size: 0.85em; box-sizing: border-box;" <>
                            if not isSelected then " opacity: 0.5;" else ""
                        , HP.placeholder "min"
                        ]
                    , HH.span
                        [ HP.style "margin: 0 2px; color: #666;" ]
                        [ HH.text "/" ]
                    , HH.input
                        [ HP.type_ HP.InputNumber
                        , HP.value $ show currentValues.max
                        , HE.onValueInput \str -> case Number.fromString str of
                            Just n -> updateAction $ createSizing { min: currentValues.min, max: n }
                            Nothing -> Skip
                        , HP.disabled (not isSelected)
                        , HP.style $ "padding: 3px 5px; width: 55px; font-size: 0.85em; box-sizing: border-box;" <>
                            if not isSelected then " opacity: 0.5;" else ""
                        , HP.placeholder "max"
                        ]
                    ]
                ]

        -- Type checkers and value extractors
        isFixed = getFixed >>> isJust
        getFixed =  case _ of
            PT.Fixed n -> Just n
            _ -> Nothing

        isPercentage = getPercentage >>> isJust
        getPercentage =  case _ of
            PT.Percentage n -> Just $ Play.pctToNumber n
            _ -> Nothing

        isFitMin = getFitMin >>> isJust
        getFitMin = case _ of
            PT.FitMin { min } -> Just min
            _ -> Nothing

        isFitMax = getFitMax >>> isJust
        getFitMax = case _ of
            PT.FitMax { max } -> Just max
            _ -> Nothing

        isFitMinMax = getFitMinMax >>> isJust
        getFitMinMax = case _ of
            PT.FitMinMax rec -> Just rec
            _ -> Nothing

        isGrowMin = getGrowMin >>> isJust
        getGrowMin = case _ of
            PT.GrowMin { min } -> Just min
            _ -> Nothing

        toDefaultValue = fromMaybe defaultSizeValue
        toDefaultPercentage = fromMaybe 0.0
        toDefaultMinValue = fromMaybe defaultMinSizeValue
        toDefaultMaxValue = fromMaybe defaultMaxSizeValue
        toDefaultMinMaxValue = fromMaybe { min : defaultMinSizeValue, max : defaultMaxSizeValue }
    in
    HH.div
        [ HP.style "font-size: 0.8em;" ]
        [ simpleRadioOption "None" PT.None
        , radioOptionWithInput "Fixed" isFixed (getFixed >>> toDefaultValue) PT.Fixed "value"
        , radioOptionWithInput "Percent" isPercentage (getPercentage >>> map (_ * 100.0) >>> toDefaultPercentage) ((_ / 100.0) >>> Play.pct >>> PT.Percentage) "value"
        , simpleRadioOption "Fit" PT.Fit
        , radioOptionWithInput "FitMin" isFitMin (getFitMin >>> toDefaultMinValue) (\min -> PT.FitMin { min }) "min"
        , radioOptionWithInput "FitMax" isFitMax (getFitMax >>> toDefaultMaxValue) (\max -> PT.FitMax { max }) "max"
        , radioOptionWithMinMax "FitMinMax" isFitMinMax (getFitMinMax >>> toDefaultMinMaxValue) PT.FitMinMax
        , simpleRadioOption "Grow" PT.Grow
        , simpleRadioOption "FitGrow" PT.FitGrow
        , radioOptionWithInput "GrowMin" isGrowMin (getGrowMin >>> toDefaultMinValue) (\min -> PT.GrowMin { min }) "min"
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
        mbCurrentPlayTree = Play.playAt state.selectedPath state.playTree
        codeContent = fromMaybe "-" $ Play.toCode (itemName >>> show) <$> mbCurrentPlayTree
        jsonContent = fromMaybe "-" $ Play.toPrettyJSON 2 <$> mbCurrentPlayTree
        arrowSymbol = if state.codePanel.expanded then "▼" else "▶"

        collapsedStyle = "font-family: monospace; position: fixed; bottom: 20px; left: 20px; z-index: 1000; background: seagreen; color: white; border: none; border-radius: 10%; padding: 10px; cursor: pointer; box-shadow: 0 4px 8px rgba(0,0,0,0.2); display: flex; align-items: center; justify-content: center; font-size: 24px; transition: all 0.3s ease;"
        expandedPanelStyle = "position: fixed; bottom: 0; left: 20%; right: 50%; height: 40vh; z-index: 1000; background: #f0f0f0; border: 2px solid #ccc; border-radius: 8px 8px 0 0; box-shadow: 0 -4px 12px rgba(0,0,0,0.15);"

        titleStyle = "padding: 8px 15px; font-weight: bold; border-bottom: 1px solid #ccc; cursor: pointer; user-select: none; display: flex; align-items: center; gap: 8px;"

        tabStyle isActive = "padding: 8px 16px; cursor: pointer; border-bottom: " <> (if isActive then "2px solid #007bff" else "1px solid #ccc") <> "; background: " <> (if isActive then "#fff" else "#f8f8f8") <> ";"

        -- Updated content style to accommodate navigation bar
        contentStyle = "height: calc(100% - 120px); overflow: auto;"
        contentWithoutNavStyle = "height: calc(100% - 80px); overflow: auto;"

        isExpanded = state.codePanel.expanded

        -- Helper functions for navigation state
        hasPreviousSibling =
            case Array.unsnoc state.selectedPath of
                Nothing -> false
                Just { last: currentIndex } -> currentIndex > 0

        hasNextSibling =
            case Array.unsnoc state.selectedPath of
                Nothing -> false
                Just { init: parentPath, last: currentIndex } ->
                    let
                        mbParentTree = Play.playAt parentPath state.playTree
                        siblingCount = fromMaybe 0 $ Array.length <$> Tree.children <$> Play.toTree <$> mbParentTree
                    in currentIndex < siblingCount - 1

        isAtRoot = state.selectedPath == []

        -- Compact navigation button style
        navButtonStyle enabled = "padding: 3px 8px; font-size: 11px; color: white; border: none; border-radius: 3px; cursor: " <>
            (if enabled then "pointer; background: #007bff;" else "not-allowed; background: #6c757d; opacity: 0.6;")

        -- Render navigation bar
        renderNavBar =
            HH.div
                [ HP.style "padding: 6px 10px; border-bottom: 1px solid #ddd; background: #f8f8f8; display: flex; gap: 6px; align-items: center;" ]
                [ HH.button
                    [ HE.onClick \_ -> GoToRoot
                    , HP.style $ navButtonStyle (not isAtRoot)
                    , HP.disabled isAtRoot
                    , HP.title "Go to root"
                    ]
                    [ HH.text "⌂" ]
                , HH.button
                    [ HE.onClick \_ -> SelectItem $ Array.dropEnd 1 state.selectedPath
                    , HP.style $ navButtonStyle (not isAtRoot)
                    , HP.disabled isAtRoot
                    , HP.title "Go to parent"
                    ]
                    [ HH.text "↑" ]
                , HH.button
                    [ HE.onClick \_ -> GoToPreviousSibling
                    , HP.style $ navButtonStyle hasPreviousSibling
                    , HP.disabled (not hasPreviousSibling)
                    , HP.title "Previous sibling"
                    ]
                    [ HH.text "←" ]
                , HH.button
                    [ HE.onClick \_ -> GoToNextSibling
                    , HP.style $ navButtonStyle hasNextSibling
                    , HP.disabled (not hasNextSibling)
                    , HP.title "Next sibling"
                    ]
                    [ HH.text "→" ]
                , HH.span
                    [ HP.style "margin-left: auto; color: #666; font-size: 10px; font-family: monospace;" ]
                    [ HH.text $ "Path: " <> show state.selectedPath ]
                ]
    in
        if isExpanded then
            HH.div
                [ HP.style expandedPanelStyle ]
                [ HH.div
                    [ HP.style titleStyle
                    , HE.onClick \_ -> ToggleCodePanel
                    ]
                    [ HH.span_ [ HH.text arrowSymbol ]
                    , HH.text "Code & Tree Viewer"
                    ]
                , HH.div
                    [ HP.style "display: flex; border-bottom: 1px solid #ccc;" ]
                    [ HH.div
                        [ HP.style $ tabStyle (state.codePanel.tabIndex == 0)
                        , HE.onClick \_ -> SelectCodeTab 0
                        ]
                        [ HH.text "Tree" ]
                    , HH.div
                        [ HP.style $ tabStyle (state.codePanel.tabIndex == 1)
                        , HE.onClick \_ -> SelectCodeTab 1
                        ]
                        [ HH.text "Code" ]
                    , HH.div
                        [ HP.style $ tabStyle (state.codePanel.tabIndex == 2)
                        , HE.onClick \_ -> SelectCodeTab 2
                        ]
                        [ HH.text "JSON" ]
                    ]
                , case state.codePanel.tabIndex of
                    0 ->  -- Tree tab (no navigation bar)
                        HH.div
                            [ HP.style contentWithoutNavStyle ]
                            [ renderTextualTree state.selectedPath state.codePanel.collapsedNodes state.playTree ]

                    1 ->  -- Code tab (with navigation bar)
                        HH.div
                            [ HP.style "height: 100%;" ]
                            [ renderNavBar
                            , HH.div
                                [ HP.style contentStyle ]
                                [ HH.textarea
                                    [ HP.value codeContent
                                    , HP.style "width: 100%; height: 100%; font-family: 'Courier New', Courier, monospace; font-size: 12px; background: #f0f0f0; border: none; padding: 10px; box-sizing: border-box; resize: none; outline: none;"
                                    , HP.readOnly true
                                    ]
                                ]
                            ]

                    2 ->  -- JSON tab (with navigation bar)
                        HH.div
                            [ HP.style "height: 100%;" ]
                            [ renderNavBar
                            , HH.div
                                [ HP.style contentStyle ]
                                [ HH.textarea
                                    [ HP.value jsonContent
                                    , HP.style "width: 100%; height: 100%; font-family: 'Courier New', Courier, monospace; font-size: 12px; background: #f0f0f0; border: none; padding: 10px; box-sizing: border-box; resize: none; outline: none;"
                                    , HP.readOnly true
                                    ]
                                ]
                            ]

                    _ -> HH.text ""
                ]
        else
            -- Collapsed: circular button with icons
            HH.button
                [ HP.style collapsedStyle
                , HE.onClick \_ -> ToggleCodePanel
                , HP.title "Open Code & Tree Viewer"
                ]
                [ HH.div
                    [ HP.style "display: flex; flex-direction: column; align-items: center; justify-content: center; line-height: 1;" ]
                    $ (\name -> HH.span [ HP.style "font-size: 16px; margin: 2px 4px;" ] [ HH.text name ]) <$>
                    [ "◈ Tree", "◈ Code", "◈ JSON" ]
                ]


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
            ) selectedExamples
        ]

-- Render interactive preview
renderClickablePreview :: forall i. State -> HH.HTML i Action
renderClickablePreview state =
    let
        layout = Play.layout state.playTree
        layoutTree = Play.layoutToTree_ layout
        size = Play.layoutSize layout
    in
    HH.div_
        [ HH.div
            [ HP.style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;" ]
            [ HH.h3
                [ HP.style "margin: 0;" ]
                [ HH.text $ fromMaybe "Interactive Preview" state.exampleName ]
            , HH.label
                [ HP.style "display: flex; align-items: center; gap: 5px; cursor: pointer; font-size: 14px;" ]
                [ HH.input
                    [ HP.type_ HP.InputCheckbox
                    , HP.checked state.showEncodedSizing
                    , HE.onChecked \_ -> ToggleShowEncodedSizing
                    ]
                , HH.text "Show encoded sizing"
                ]
            ]
        , HS.svg
            [ HA.width size.width
            , HA.height size.height
            , HP.style "border: 1px solid #ccc;"
            ]
            $ renderClickableItem state <$>
                (Tree.flatten
                    $ map (lmap Tree.Path.toArray)
                    $ Tree.Path.fill
                    $ layoutTree
                )
        ]


renderClickableItem :: forall i item. RenderItem item => State -> (Play.ItemPath /\ PT.WithDefRect (ItemWithChanges item)) -> HH.HTML i Action
renderClickableItem state (path /\ { v, def, rect }) =
    let
        isSelected = state.selectedPath == path
        labelText = Play.encodeDef def
        mbCol = itemColor v
    in HS.g
        [ HE.onClick \_ -> SelectItem path
        , HP.style "pointer-events: all; cursor: pointer;"
        ]
        $ [ HS.rect
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
        , Demo.renderItem (SelectItem path) { v, rect }
        ]
        <> if state.showEncodedSizing then
            [ HS.text
                [ HA.x $ rect.pos.x + 5.0
                , HA.y $ rect.pos.y + 23.0
                , HA.fontSize $ HA.FontSizeLength $ HA.Px 10.0
                , HA.fill $ HA.Named "white"
                , HA.strokeWidth 0.3
                , HA.dominantBaseline HA.Hanging
                , HP.style "pointer-events: none; opacity: 0.8;"
                , HE.onClick \_ -> SelectItem path
                ]
                [ HH.text labelText ]
            ]
            else []


renderTextualTree :: forall i item. IsItem item => Play.ItemPath -> Array Play.ItemPath -> Play item -> HH.HTML i Action
renderTextualTree selectedPath collapsedNodes playTree =
  let
    tree = Play.toTree playTree
  in
    HH.div
      [ HP.style "width: 100%; height: 100%; font-family: 'Courier New', Courier, monospace; font-size: 12px; background: #f0f0f0; border: none; padding: 5px; box-sizing: border-box; margin: 0; overflow: auto;"
      ]
      [ renderTextualTreeNode [] selectedPath collapsedNodes tree ]


renderTextualTreeNode
    :: forall i item
     . IsItem item
    => Play.ItemPath
    -> Play.ItemPath
    -> Array Play.ItemPath
    -> Tree (PT.WithDef item)
    -> HH.HTML i Action
renderTextualTreeNode currentPath selectedPath collapsedNodes tree =
  let
    isSelected = currentPath == selectedPath
    isCollapsed = Array.elem currentPath collapsedNodes
    hasChildren = not $ Array.null $ Tree.children tree
    item = Tree.value tree
    itemLabel = if itemName item.v == "" then "<?>" else itemName item.v
    depth = Array.length currentPath
    indent = String.fromCodePointArray (Array.replicate (depth * 4) (String.codePointFromChar ' '))

    expandSymbol = if hasChildren
      then (if isCollapsed then "[+]" else "[-]")
      else " - "

    highlighted = if isSelected
      then "[" <> itemLabel <> "]"
      else itemLabel

    childrenNodes = if hasChildren && not isCollapsed
      then
        Array.mapWithIndex (\i ->
            renderTextualTreeNode (Array.snoc currentPath i) selectedPath collapsedNodes
        ) $ Tree.children tree
      else []

    encodedDef = Play.encodeDef item.def
  in
    HH.div_ $
        [ HH.div
          [ HP.style $ "padding: 2px 0; " <> (if isSelected then "background-color: #e0e0e0;" else "")
          ]
          [ HH.span [ HP.style "white-space: pre;" ] [ HH.text indent ]
          , HH.span
                [ HP.style "white-space: pre; user-select: none; cursor: pointer; display: inline-block; width: 20px; margin-right: 5px;"
                , HE.onClick \_ -> if hasChildren then ToggleNodeCollapsed currentPath else SelectItem currentPath
                ]
                [ HH.text expandSymbol ]
          , HH.span
                [ HP.style $ "user-select: none; cursor: pointer;"
                , HE.onClick \_ -> SelectItem currentPath
                ]
                [ HH.text $ highlighted ]
        , HH.span
                [ HP.style "font-weight: 300; color: #888; margin-left: 8px; font-size: 0.8em;"
                , HE.onClick \_ -> SelectItem currentPath
                ]
                [ HH.text encodedDef ]
          ]
      ] <> childrenNodes


initChanges :: forall x. IsItem x => x -> ItemWithChanges x
initChanges item =
    IWC
        { item : item
        , changes :
            { name : itemName item
            , mbColor : itemColor item
            }
        }


instance IsItem (ItemWithChanges x) where
    itemName  (IWC { changes }) = changes.name
    itemColor (IWC { changes }) = changes.mbColor


instance (IsItem x, NextItem x) => NextItem (ItemWithChanges x) where
    nextItem = nextItem >>> initChanges


instance RenderItem x => RenderItem (ItemWithChanges x) where
    renderItem action { v, rect } =
        case v of
            IWC { item, changes } ->
                renderItem action { v : item, rect }
        -- Demo.renderItem action { v: v.changes, rect }


instance WriteForeign (ItemWithChanges x) where
    writeImpl (IWC { changes }) = writeImpl $ changes.name
    -- itemName >>> writeImpl