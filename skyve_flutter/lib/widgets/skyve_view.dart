import 'package:flutter/material.dart';
import 'package:nested_scroll_view_plus/nested_scroll_view_plus.dart';
import 'package:skyve_flutter/util/responsive_grid.dart';
import '../util/skyve_flutter_form.dart';
import 'skyve_menu.dart';

String nvl(dynamic value) {
  return (value == null) ? '' : value.toString();
}

abstract class SkyveView {
  // Produces the widgets for the action bar.
  List<Widget> actions(BuildContext context);

  // Produces the widgets for the view.
  List<Widget> contained(BuildContext context);

  // Used to track the screen size from the layout builder in responsiveView below
  // This is preferrable to using MediaQuery which has the potential to trigger unnecessary paints.
  static Size screenSize = const Size(1, 1); // ND no div 0

  // Is the width < The 'sm' breakpoint;
  static bool small = false;

  static Widget responsiveView(BuildContext context, String viewTitle,
      Widget view, PreferredSize? appBarBottomContents) {
    return LayoutBuilder(builder: (context, constraints) {
      screenSize = Size(constraints.maxWidth, constraints.maxHeight);
      small = (screenSize.width <= ResponsiveWidth.maxSmallScreenWidthPixels);
      final Drawer? drawer =
          small ? const Drawer(child: SkyveMenu(inDrawer: true)) : null;
      Widget body;
      if (small) {
        body = view;
      } else {
        body = Row(children: [
          const SizedBox(
              width: 240,
              child: Column(
                  children: [Expanded(child: SkyveMenu(inDrawer: false))])),
          Expanded(child: view)
        ]);
      }
      ThemeData theme = Theme.of(context);

      return Scaffold(
          drawer: drawer,
          // NB Use NestedScrollViewPlus to allow over-stretch of SliverAppBar
// TODO In chrome having scrolling menu inlined causes - The provided ScrollController is currently attached to more than one ScrollPosition.
          body: NestedScrollViewPlus(
              headerSliverBuilder: (context, innerBoxIsScrolled) {
                return [
                  SliverAppBar(
                    pinned: true,
                    snap: true,
                    floating: true,
                    stretch: true,
//                    actions: [], TODO for RHS actions
                    expandedHeight: small ? 120.0 : 160.0,
// TODO use a leading icon when making this thing and the title and then add the tab bar in below
                    bottom: appBarBottomContents,
                    flexibleSpace: FlexibleSpaceBar(
                        title: Text(viewTitle),
                        stretchModes: const [
                          StretchMode.zoomBackground,
                          StretchMode.blurBackground,
                        ],
                        background: Stack(fit: StackFit.expand, children: [
                          DecoratedBox(
                            position: DecorationPosition.foreground,
                            decoration: BoxDecoration(
                                gradient: LinearGradient(
                                    colors: [
                                  theme.primaryColor,
                                  Colors.transparent
                                ],
                                    begin: Alignment.bottomCenter,
                                    end: Alignment.topCenter)),
                            child: const Image(
                              image: AssetImage('assets/AppBarBackground.jpg'),
                              fit: BoxFit.cover,
                            ),
                          ),
                          Container(
                              padding: const EdgeInsets.all(32.0),
                              child: const Image(
                                image: AssetImage('assets/skyve_inv_logo.png'),
                                fit: BoxFit.contain,
                                opacity: AlwaysStoppedAnimation(0.3),
                              ))
                        ])),
                  )
                ];
              },
              body: SkyveFlutterForm(
                child: Form(
                  child: body,
                ),
              )),
// TODO Put the actions in here
          bottomNavigationBar: BottomAppBar(
              child: Row(
                  mainAxisSize: MainAxisSize.max,
                  mainAxisAlignment: MainAxisAlignment.spaceBetween,
                  children: [
                IconButton(
                  icon: const Icon(Icons.menu),
                  onPressed: () {},
                ),
                IconButton(
                  icon: const Icon(Icons.search),
                  onPressed: () {},
                ),
              ])));
    });
  }
}
