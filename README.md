# purescript-play

UI layout system, as simple and minimal as possible, inspired by Clay (C Layout) from Nic Barker: [YouTube Video](https://www.youtube.com/watch?v=by9lQvpvMIc).

No text measurement (since the library is not bound to any UI engine, web or server) and so no auto-fit support yet.

* [Detailed blog post in my own blog](https://shamansir.github.io/Blog/blog-en/aa-2026-01-11-play-ui-layouts-in-purescript.html)
* [Detailed blog post at dev.to](https://dev.to/shamansir/play-ui-layouts-in-purescript-1cb5)
* Quick overview with `test/Demo` module running: [as YT shorts](https://youtube.com/shorts/cRGQw67-7FQ).
* Constructor Demo: https://shamansir.github.io/purescript-play/constructor.html

Pursuit Page: https://pursuit.purescript.org/packages/purescript-play/

# Developing with NixOS

Just use `nix develop` in the repository and everything should work.

# Examples:

Menu with items:

```purescript
import Play (Play, (~*))
import Play as Play

menuUI =
    Play.i "menu"
        ~* Play.width 250.0
        ~* Play.heightFit
        ~* (Play.padding $ Play.all 5.0)
        ~* Play.topToBottom
        ~* Play.childGap 5.0
        ~* Play.with (menuItem <$> [ "Copy", "Paste", "Delete", "Spell Check", "Dictionary", "Comment" ])
    :: Play String

menuItem :: String -> Play Item
menuItem itemName =
    Play.i ("item-" <> itemName)
        ~* Play.widthGrow
        ~* Play.heightFit
        ~* Play.leftToRight
        ~* (Play.padding $ Play.all 3.0)
        ~* Play.with
        [ Play.i itemName
            ~* Play.widthGrow
            ~* Play.height 60.0
        , Play.i (ic (HA.RGB 94 64 157) "icon")
            ~* Play.width 60.0
            ~* Play.height 60.0
        ]
```

Noodle Horizontal Node UI:

```purescript
-- surely you can use sum-type here instead
data NItem = NItem Color String

ic :: Color -> String -> NItem
ic = NItem


noodleHorzNodeUI :: Play NItem
noodleHorzNodeUI =
    let
        titleWidth = 30.0
        channelsHeight = 25.0
        bodyWidth = 700.0
        bodyHeight = 120.0
        channelWidth = 70.0
        connectorWidth = 15.0
        inletsCount = 5
        outletsCount = 7

        inlet n =
            Play.i (ic (HA.Named "transparent") "")
            ~* Play.width channelWidth
            ~* Play.heightGrow
            ~* Play.with
                [ Play.i (ic (HA.RGB 83 105 7) "connector")
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                , Play.i (ic (HA.RGB 175 48 41) $ show n <> " inlet")
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                ]
        inlets = inlet <$> Array.range 0 inletsCount

        outlet n =
            Play.i (ic (HA.Named "transparent") "")
            ~* Play.width channelWidth
            ~* Play.heightGrow
            ~* Play.with
                [ Play.i (ic (HA.RGB 83 105 7) "connector")
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                , Play.i (ic (HA.RGB 175 48 41) $ show n <> " outlet")
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                ]
        outlets = outlet <$> Array.range 0 outletsCount

    in Play.i (ic (HA.Named "blue") "background")
        ~* Play.widthFit
        ~* Play.heightFit
        ~* Play.leftToRight
        ~* Play.with
            [ Play.i (ic (HA.Named "magenta") "title + paddings")
                ~* Play.width titleWidth
                ~* Play.heightFit
                ~* Play.topToBottom
            ~* Play.with
                [ Play.i (ic (HA.RGB 32 94 166) "padding top")
                    ~* Play.widthGrow
                    ~* Play.height channelsHeight
                , Play.i (ic (HA.Named "black") "title")
                    ~* Play.widthGrow
                    ~* Play.height bodyHeight
                , Play.i (ic (HA.RGB 32 94 166) "padding bottom")
                    ~* Play.widthGrow
                    ~* Play.height channelsHeight
                ]
            , Play.i (ic (HA.RGB 49 35 78) "") -- inlets + body + outlets
                ~* Play.widthFit
                ~* Play.heightFit
                ~* Play.topToBottom
                ~* Play.with
                    [ Play.i (ic (HA.RGB 79 27 57) "inlets")
                        ~* Play.widthFit
                        ~* Play.height channelsHeight
                        ~* Play.with inlets
                    , Play.i (ic (HA.Named "lightgray") "body bg")
                        ~* Play.widthFitGrow
                        ~* Play.height bodyHeight
                        ~* Play.with
                            [ Play.i (ic (HA.RGB 90 189 172) "body")
                                ~* Play.width  bodyWidth
                                ~* Play.height bodyHeight
                            ]
                    , Play.i (ic (HA.RGB 79 27 57) "outlets")
                        ~* Play.widthFit
                        ~* Play.height channelsHeight
                        ~* Play.with outlets
                    ]
                ]
```

Noodle Node UI alternative definition:

```purescript
data NodePart
    = Title
    | TitleArea -- Title + Paddings
    | ControlButton
    | TitlePadding
    | Inlet Int InletDefRec
    | InletName
    | InletConnector
    | Outlet Int OutletDefRec
    | OutletName
    | OutletConnector
    | NodeBackground
    | BodyArea -- Body + Inlets + Outltets
    | Inlets
    | Outlets
    | BodyBackground
    | Body


type NodeParams =
    { inlets :: Array InletDefRec
    , outlets :: Array OutletDefRec
    , bodyWidth :: Number
    , bodyHeight :: Number
    }


horzNodeUI :: NodeParams -> Play NodePart
horzNodeUI params =
    let
        titleWidth = 16.0
        channelsHeight = 20.0
        channelWidth = 70.0
        connectorWidth = 15.0

        inlet n def =
            Play.i (Inlet n def)
            ~* Play.width channelWidth
            ~* Play.heightGrow
            ~* Play.with
                [ Play.i InletConnector
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                , Play.i InletName
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                ]
        inlets = mapWithIndex inlet params.inlets

        outlet n def =
            Play.i (Outlet n def)
            ~* Play.width channelWidth
            ~* Play.heightGrow
            ~* Play.with
                [ Play.i OutletConnector
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                , Play.i OutletName
                    ~* Play.widthGrow
                    ~* Play.heightGrow
                ]
        outlets = mapWithIndex outlet params.outlets

    in Play.i NodeBackground
        ~* Play.widthFit
        ~* Play.heightFit
        ~* Play.leftToRight
        ~* Play.with
            [ Play.i TitleArea
                ~* Play.width titleWidth
                ~* Play.heightFit
                ~* Play.topToBottom
            ~* Play.with
                [ Play.i ControlButton
                    ~* Play.widthGrow
                    ~* Play.height channelsHeight
                , Play.i Title
                    ~* Play.widthGrow
                    ~* Play.height params.bodyHeight
                , Play.i TitlePadding
                    ~* Play.widthGrow
                    ~* Play.height channelsHeight
                ]
            , Play.i BodyArea
                ~* Play.widthFit
                ~* Play.heightFit
                ~* Play.topToBottom
                ~* Play.padding { top : 0.0, left : 5.0, right : 0.0, bottom : 0.0 }
                ~* Play.with
                    [ Play.i Inlets
                        ~* Play.widthFit
                        ~* Play.height channelsHeight
                        ~* Play.with inlets
                    , Play.i BodyBackground
                        ~* Play.widthFitGrow
                        ~* Play.height params.bodyHeight
                        ~* Play.with
                            [ Play.i Body
                                ~* Play.width  params.bodyWidth
                                ~* Play.height params.bodyHeight
                            ]
                    , Play.i Outlets
                        ~* Play.widthFit
                        ~* Play.height channelsHeight
                        ~* Play.with outlets
                    ]
                ]


vertNodeUI :: NodeParams -> Play NodePart
vertNodeUI params =
    let
        titleHeight = 30.0
        channelNameMinWidth = 100.0
        paddingWidth = channelNameMinWidth + connectorWidth
        channelHeight = 20.0
        connectorWidth = 15.0

        inlet n def =
            Play.i (Inlet n def)
            ~* Play.widthFit
            ~* Play.heightFit
            ~* Play.with
                [ Play.i InletName
                    ~* Play.width  channelNameMinWidth
                    ~* Play.height channelHeight
                , Play.i InletConnector
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                ]
        inlets = mapWithIndex inlet params.inlets

        outlet n def =
            Play.i (Outlet n def)
            ~* Play.widthFit
            ~* Play.heightFit
            ~* Play.with
                [ Play.i OutletConnector
                    ~* Play.width connectorWidth
                    ~* Play.heightGrow
                , Play.i OutletName
                    ~* (Play.width  channelNameMinWidth)
                    ~* (Play.height channelHeight)
                ]
        outlets = mapWithIndex outlet params.outlets

    in Play.i NodeBackground
        ~* Play.widthFit
        ~* Play.heightFit
        ~* Play.topToBottom
        ~* Play.with
            [ Play.i TitleArea
                ~* Play.widthFit
                ~* Play.height titleHeight
                ~* Play.leftToRight
                ~* Play.with
                    [ Play.i TitlePadding
                        ~* Play.width paddingWidth
                        ~* Play.heightGrow
                    , Play.i Title
                        ~* Play.width  params.bodyWidth
                        ~* Play.height titleHeight
                    , Play.i TitlePadding
                        ~* Play.width paddingWidth
                        ~* Play.heightGrow
                    ]

            , Play.i BodyArea
                ~* Play.widthFit
                ~* Play.heightFit
                ~* Play.leftToRight
                ~* Play.with
                    [ Play.i Inlets
                        ~* Play.widthFit
                        ~* Play.heightFit
                        ~* Play.topToBottom
                        ~* Play.with inlets
                    , Play.i BodyBackground
                        ~* Play.width params.bodyWidth
                        ~* Play.heightFitGrow
                        ~* Play.with
                            [ Play.i Body
                                ~* Play.width  params.bodyWidth
                                ~* Play.height params.bodyHeight
                            ]
                    , Play.i Outlets
                        ~* Play.widthFit
                        ~* Play.heightFit
                        ~* Play.topToBottom
                        ~* Play.with outlets
                    ]
            ]
```


You may find much more examples in [`Test.Demo.Examples` source](https://github.com/shamansir/purescript-play/blob/main/test/Demo/Examples.purs).

Or just take a look at a code that is generated with Constructor.

# Calculating Positions

To convert your UI definition to the layout definition, use `Play.layout` function:

```purescript
menuLayout = Play.layout menuUI :: Play.Layout String
noodleHorzNodeLayout = Play.layout noodleHorzUI :: Play.Layout NItem
```

The `Layout a` can be converted to an array of items with positions `Array (Play.WithRect a)` with `Play.flattenLayout`,
or  you may prefer`Play.layoutToTree` to get a `Tree (Play.WithRect a)` if you want to keep all the parent-child relations
(we use [`Yoga.Tree`](https://pursuit.purescript.org/packages/purescript-yoga-tree/1.0.0/docs/Yoga.Tree#t:Tree) to represent them here).


```purescript
menuPositionedItems = Play.flattenLayout menuLayout :: Array (Play.WithRect String)
menuPositionedTree  = Play.layoutToTree  menuLayout :: Yoga.Tree (Play.WithRect String)

noodleHorzNodeItems = Play.flattenLayout noodleHorzNodeLayout :: Array (Play.WithRect NItem)
noodleHorzNodeTree  = Play.layoutToTree  noodleHorzNodeLayout :: Yoga.Tree (Play.WithRect NItem)
```

# TODO

* Evenly distribute children over the parent area (a.k.a flex layout)
    * Examples
* Support associating custom values in the constructor

