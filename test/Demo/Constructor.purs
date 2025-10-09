module Demo.Constructor where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Int as Int
import Data.Map (update)
import Data.Maybe (Maybe(..), fromMaybe, maybe, isJust)
import Data.Number as Number
import Data.String as String
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Halogen as H
import Halogen.Aff (runHalogenAff, awaitBody) as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as HA
import Halogen.Svg.Elements as HS
import Halogen.VDom.Driver (runUI)
import Play (Play, with)
import Play as Play
import Play.Types (Padding, Sizing(..), Direction(..), Def, WithDef, WithRect) as PT
import Test.Demo (renderOne) as Demo
import Test.Demo.Examples (Example, layoutExample, playOf, itemName, noodleUI, Item(..), il, ic)
import Web.DOM.Text (wholeText)
import Yoga.Tree (Tree)
import Yoga.Tree.Extended (node, break, flatten, value, children, update) as Tree
import Yoga.Tree.Extended.Path (Path(..)) as Tree
import Yoga.Tree.Extended.Path as Tree.Path

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body

-- Path to an item in the tree (array of child indices)
type ItemPath = Array Int


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
    | ResetFixedWidth
    | ResetFixedHeight
    | AddChild String
    | RemoveChild Int


type EditingState =
    { name :: String
    , def :: PT.Def
    , color :: Maybe HA.Color
    , fixed ::
        { width :: Maybe Number
        , height :: Maybe Number
        }
    }


type State =
    { playTree :: Play Item
    , selectedPath :: ItemPath
    , editing :: EditingState
    }


-- Helper functions for default values
defaultColor :: HA.Color
defaultColor = HA.RGB 128 128 128


itemToString :: Item -> String
itemToString (Item mbCol name) = case mbCol of
    Just (HA.RGB r g b) -> "(ic (HA.RGB " <> show r <> " " <> show g <> " " <> show b <> ") " <> show name <> ")"
    Just (HA.RGBA r g b a) -> "(ic (HA.RGBA " <> show r <> " " <> show g <> " " <> show b <> " " <> show a <> ") " <> show name <> ")"
    Just (HA.Named n) -> "(ic (HA.Named " <> show n <> ") " <> show name <> ")"
    Just HA.NoColor -> "(ic HA.NoColor " <> show name <> ")"
    Nothing -> "(il " <> show name <> ")"


-- Get item at a specific path in the tree
getItemAtPath :: ItemPath -> Play Item -> Maybe Item
getItemAtPath path =
    Play.toTree
    >>> Tree.Path.find (Tree.Path path)
    >>> map (Tree.value >>> _.v)


-- Get definition at a specific path in the tree
getDefAtPath :: ItemPath -> Play Item -> Maybe PT.Def
getDefAtPath path =
    Play.toTree
    >>> Tree.Path.find (Tree.Path path)
    >>> map (Tree.value >>> _.def)


-- Get subtree at path
getSubtreeAtPath :: ItemPath -> Play Item -> Maybe (Play Item)
getSubtreeAtPath path =
    Play.toTree
    >>> Tree.Path.find (Tree.Path path)
    >>> map Play.fromTree


-- Update item at path
updateItemAtPath :: ItemPath -> (Item -> Item) -> Play Item -> Play Item
updateItemAtPath path updateFn =
    updateWithDefAtPath path \wd -> wd { v = updateFn wd.v }


-- Update definition at path
updateDefAtPath :: ItemPath -> (PT.Def -> PT.Def) -> Play Item -> Play Item
updateDefAtPath path updateFn =
    updateWithDefAtPath path \wd -> wd { def = updateFn wd.def }


updateWithDefAtPath :: ItemPath -> (PT.WithDef Item -> PT.WithDef Item) -> Play Item -> Play Item
updateWithDefAtPath path updateFn playTree =
    Play.fromTree $ Tree.Path.with (Tree.Path path) (Tree.update updateFn) $ Play.toTree playTree

-- Set item name
setItemName :: String -> Item -> Item
setItemName newName (Item mbCol _) = Item mbCol newName

-- Set item color
setItemColor :: HA.Color -> Item -> Item
setItemColor newColor (Item _ name) = Item (Just newColor) name

-- Add child at path
addChildAtPath :: ItemPath -> Play Item -> Play Item -> Play Item
addChildAtPath path newChild playTree =
    Play.fromTree $ addChildInTree path (Play.toTree newChild) (Play.toTree playTree)

addChildInTree :: ItemPath -> Tree (PT.WithDef Item) -> Tree (PT.WithDef Item) -> Tree (PT.WithDef Item)
addChildInTree [] newChild tree =
    let children = Tree.children tree
    in Tree.node (Tree.value tree) (Array.snoc children newChild)
addChildInTree path newChild tree = case Array.uncons path of
    Just { head: index, tail: rest } ->
        let children = Tree.children tree
            updatedchildren = Array.modifyAt index (addChildInTree rest newChild) children
        in Tree.node (Tree.value tree) (fromMaybe children updatedchildren)
    Nothing -> tree

-- Remove child at path
removeChildAtPath :: ItemPath -> Int -> Play Item -> Play Item
removeChildAtPath path childIndex playTree =
    Play.fromTree $ removeChildInTree path childIndex (Play.toTree playTree)

removeChildInTree :: ItemPath -> Int -> Tree (PT.WithDef Item) -> Tree (PT.WithDef Item)
removeChildInTree [] childIndex tree =
    let children = Tree.children tree
        updatedchildren = fromMaybe children (Array.deleteAt childIndex children)
    in Tree.node (Tree.value tree) updatedchildren
removeChildInTree path childIndex tree = case Array.uncons path of
    Just { head: index, tail: rest } ->
        let children = Tree.children tree
            updatedchildren = Array.modifyAt index (removeChildInTree rest childIndex) children
        in Tree.node (Tree.value tree) (fromMaybe children updatedchildren)
    Nothing -> tree


isFixedSizing :: PT.Sizing -> Maybe Number
isFixedSizing = case _ of
    PT.Fixed n -> Just n
    _ -> Nothing


-- Render property editor
renderPropertyEditor :: forall i. State -> HH.HTML i Action
renderPropertyEditor state =
    let
        childrenCount = fromMaybe 0 $ Array.length <$> Tree.children <$> Play.toTree <$> getSubtreeAtPath state.selectedPath state.playTree
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
            [ HH.h4_ [ HH.text $ "Children (" <> show childrenCount <> "):" ]
            , HH.div_
                (Array.mapWithIndex (\i child ->
                    HH.div
                        [ HP.style "display: flex; align-items: center; gap: 10px; margin: 5px 0;" ]
                        [ HH.span_ [ HH.text $ show i <> ". " <> fromMaybe "?" (itemName <$> getItemAtPath (Array.snoc state.selectedPath i) state.playTree) ]
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
                ) $ Array.range 0 (childrenCount - 1))
            , HH.div
                [ HP.style "margin-top: 10px;" ]
                [ HH.input
                    [ HP.type_ HP.InputText
                    , HP.placeholder "New child name"
                    , HP.id "new-child-name"
                    , HP.style "padding: 5px; margin-right: 10px;"
                    ]
                , HH.button
                    [ HE.onClick \_ -> do
                        let newName = "New Child" -- TODO: get from input
                        AddChild newName
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
        defaultValue = 100.0
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
                    then updateAction $ PT.Fixed $ fromMaybe defaultValue mbEditingValue
                    else updateAction $ PT.None
                , HP.id "fixed-checkbox"
                ]
            , HH.label
                [ HP.for "fixed-checkbox"
                , HP.style "margin: 0;"
                ]
                [ HH.text "Fixed size:" ]
            , HH.input
                [ HP.type_ HP.InputNumber
                , HP.value $ show $ fromMaybe defaultValue mbEditingValue
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
    HH.select
        [ HE.onSelectedIndexChange \idx -> case idx of
            0 -> Skip
            1 -> UpdateColor $ HA.RGB 255 0 0
            2 -> UpdateColor $ HA.RGB 0 255 0
            3 -> UpdateColor $ HA.RGB 0 0 255
            4 -> UpdateColor $ HA.Named "pink"
            5 -> UpdateColor $ HA.Named "yellow"
            6 -> UpdateColor $ HA.Named "lightblue"
            7 -> UpdateColor $ HA.RGB 128 128 128
            _ -> Skip
        , HP.style "width: 100%; padding: 5px; margin-top: 5px;"
        ]
        [ HH.option [ HP.selected (currentColor == Nothing) ] [ HH.text "Transparent" ]
        , HH.option [ HP.selected (currentColor == Just (HA.RGB 255 0 0)) ] [ HH.text "Red" ]
        , HH.option [ HP.selected (currentColor == Just (HA.RGB 0 255 0)) ] [ HH.text "Green" ]
        , HH.option [ HP.selected (currentColor == Just (HA.RGB 0 0 255)) ] [ HH.text "Blue" ]
        , HH.option [ HP.selected (currentColor == Just (HA.Named "pink")) ] [ HH.text "Pink" ]
        , HH.option [ HP.selected (currentColor == Just (HA.Named "yellow")) ] [ HH.text "Yellow" ]
        , HH.option [ HP.selected (currentColor == Just (HA.Named "lightblue")) ] [ HH.text "Light Blue" ]
        , HH.option [ HP.selected (currentColor == Just (HA.RGB 128 128 128)) ] [ HH.text "Gray" ]
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
        [ HH.h3_ [ HH.text "Interactive Preview" ]
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
                }

        initialState _ =
            let
                tree = playOf noodleUI
            in
                { playTree: tree
                , selectedPath: []
                , editing: loadEditState [] tree
                }

        render :: State -> _
        render state =
            HH.div
                [ HP.style "font-family: 'TeX Gyre Adventor', 'JetBrains Sans', Monaco, Helvetica, sans-serif; font-weight: 600; display: flex; gap: 20px;" ]
                [ HH.div
                    [ HP.style "flex: 1;" ]
                    [ renderPropertyEditor state
                    {-
                    [ HP.style "font-family: 'TeX Gyre Adventor', 'JetBrains Sans', Monaco, Helvetica, sans-serif; font-weight: 600;" ]
                    [ HH.textarea [ HP.value $ toCode (itemName >>> show) $ playOf currentExample
                                , HP.style "width: 100%; height: 200px; font-family: 'Courier New', Courier, monospace; font-size: 14px; background: #f0f0f0; border: 1px solid #ccc; padding: 10px; box-sizing: border-box;"
                                , HP.readOnly true
                                ]
                    -}
                    , HH.textarea
                        [ HP.value $ fromMaybe "-" $ toCode (itemName >>> show) <$> getSubtreeAtPath state.selectedPath state.playTree
                        , HP.style "width: 100%; height: 150px; font-family: 'Courier New', Courier, monospace; font-size: 12px; background: #f0f0f0; border: 1px solid #ccc; padding: 10px; box-sizing: border-box; margin-top: 10px;"
                        , HP.readOnly true
                        ]
                    ]
                , HH.div
                    [ HP.style "flex: 1;" ]
                    [ renderInteractivePreview state ]
                    -- [  ]
                    -- $ pure
                    -- $ Demo.renderOne
                    -- $ layoutExample
                    -- $ currentExample
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

            AddChild childName -> do
                state <- H.get
                let newChild = Play.i (il childName)
                    updatedTree = addChildAtPath state.selectedPath newChild state.playTree
                H.modify_ _ { playTree = updatedTree }

            RemoveChild childIndex -> do
                state <- H.get
                let updatedTree = removeChildAtPath state.selectedPath childIndex state.playTree
                H.modify_ _ { playTree = updatedTree }

            ResetFixedWidth -> do
                H.modify_ $ updateFixed $ _ { width = Nothing }

            ResetFixedHeight -> do
                H.modify_ $ updateFixed $ _ { height = Nothing }


toCode :: forall a. (a -> String) -> Play a -> String
toCode vToString = Play.toTree >>> renderTreeWithIndent ""
    where
    renderTreeWithIndent :: String -> Tree (PT.WithDef a) -> String
    renderTreeWithIndent indent t =
        let
            wd = Tree.value t :: PT.WithDef a
            def = wd.def :: PT.Def
            start = "Play.i (" <> vToString wd.v <> ")"

            renderWidth :: PT.Sizing -> Maybe String
            renderWidth = case _ of
                PT.None -> Nothing -- default
                PT.Fixed n -> Just $ "Play.width " <> show n
                PT.Fit -> Just "Play.widthFit"
                PT.Grow -> Just "Play.widthGrow"
                PT.FitGrow -> Just "Play.widthFitGrow"

            renderHeight :: PT.Sizing -> Maybe String
            renderHeight = case _ of
                PT.None -> Nothing -- default
                PT.Fixed n -> Just $ "Play.height " <> show n
                PT.Fit -> Just "Play.heightFit"
                PT.Grow -> Just "Play.heightGrow"
                PT.FitGrow -> Just "Play.heightFitGrow"

            renderPadding :: PT.Padding -> Maybe String
            renderPadding pad = if pad.top == 0.0 && pad.left == 0.0 && pad.bottom == 0.0 && pad.right == 0.0 then
                Nothing
            else if pad.top == pad.left && pad.left == pad.bottom && pad.bottom == pad.right then
                Just $ "(Play.padding $ Play.all " <> show pad.top <> ")"
            else
                Just $ "Play.padding { top : " <> show pad.top <> ", "
                                    <> "left : " <> show pad.left <> ", "
                                    <> "bottom : " <> show pad.bottom <> ", "
                                    <> "right : " <> show pad.right <> " }"

            renderDirection :: PT.Direction -> Maybe String
            renderDirection = case _ of
                PT.LeftToRight -> Nothing -- "leftToRight" is the default direction
                PT.TopToBottom -> Just "Play.topToBottom"

            renderChildGap :: Number -> Maybe String
            renderChildGap n = if n == 0.0 then Nothing else Just $ "Play.childGap " <> show n

            modifiersList = Array.catMaybes [ renderWidth def.sizing.width
                                            , renderHeight def.sizing.height
                                            , renderPadding def.padding
                                            , renderChildGap def.childGap
                                            , renderDirection def.direction
                                            , renderChildren $ Tree.children t
                                            ]

            renderChildren :: Array (Tree (PT.WithDef a)) -> Maybe String
            renderChildren arr =
                if Array.null arr then Nothing
                else
                    let
                        joined = String.joinWith ("\n" <> indent <> ", ") $ renderTreeWithIndent (indent <> "    ") <$> arr
                    in Just $ "Play.with\n" <> indent <> "[ " <> joined <> "\n" <> indent <> "]"

            joinedMods = String.joinWith ("\n" <> indent <> "~* ") modifiersList
        in if Array.null modifiersList then start else start <> " \n" <> indent <> "~* " <> joinedMods