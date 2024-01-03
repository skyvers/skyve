import 'dart:math';

import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:nested_scroll_view_plus/nested_scroll_view_plus.dart';
import 'package:skyve_flutter/util/responsive_grid.dart';
import '../util/skyve_flutter_form.dart';
import 'skyve_menu.dart';

abstract class SkyveView {
  // width of drawer/panel for menu
  static const double menuWidth = 240.0;

  // Produces the widgets for the action bar.
  List<Widget> actions(BuildContext context);

  // Produces the widgets for the view.
  List<Widget> contained(BuildContext context);

  // Used to track the screen size from the layout builder in responsiveView below
  // This is preferrable to using MediaQuery which has the potential to trigger unnecessary paints.
  static Size screenSize = const Size(1.0, 1.0); // NB no div 0

  // Is the width < The 'sm' breakpoint;
  static bool small = false;

  static Widget responsiveView(
      {required BuildContext context,
      required String viewTitle,
      required Widget view,
      PreferredSize? appBarBottomContents,
      List<Widget> actions = const []}) {
    return LayoutBuilder(builder: (context, constraints) {
      screenSize = Size(constraints.maxWidth, constraints.maxHeight);
      small = (screenSize.width <= ResponsiveWidth.maxSmallScreenWidthPixels);
      final Drawer? drawer =
          small ? const Drawer(child: SkyveMenu(inDrawer: true)) : null;

      // Determine the body widget
      Widget body;
      if (small) {
        body = view;
      } else {
        body = Row(children: [
          const SizedBox(
              width: menuWidth,
              child: Column(
                  children: [Expanded(child: SkyveMenu(inDrawer: false))])),
          Expanded(child: view)
        ]);
      }

      // Determine the bottom app bar
      // DB This is built here inside a LayoutBuilder since it is responsive
      BottomAppBar? bottomNavigationBar;
      if (actions.isNotEmpty) {
        Widget guts = SingleChildScrollView(
          scrollDirection: Axis.horizontal,
          child: Container(
            constraints: BoxConstraints(
                minWidth: SkyveView.small
                    // NB -20 for padding
                    ? SkyveView.screenSize.width - 20.0
                    : max(
                        // NB -menuWidth for left padding -10 for right padding
                        SkyveView.screenSize.width - SkyveView.menuWidth - 10.0,
                        SkyveView.menuWidth,
                      )),
            child: Wrap(
              alignment: WrapAlignment.center,
              spacing: 8.0,
              children: actions,
            ),
          ),
        );
        if (kIsWeb) {
          guts = Padding(
            padding: SkyveView.small
                ? const EdgeInsets.all(10.0)
                : const EdgeInsets.only(
                    left: SkyveView.menuWidth,
                    right: 10.0,
                    top: 10.0,
                    bottom: 10.0),
            child: guts,
          );
        } else {
          guts = Padding(
            padding: SkyveView.small
                ? const EdgeInsets.symmetric(horizontal: 10.0)
                : const EdgeInsets.only(left: SkyveView.menuWidth, right: 10.0),
            child: guts,
          );
        }
        bottomNavigationBar = BottomAppBar(child: guts);
      }

      ThemeData theme = Theme.of(context);

      return Scaffold(
          drawer: drawer,
          // NB Use NestedScrollViewPlus to allow over-stretch of SliverAppBar
// TODO In chrome having scrolling menu inlined causes - The provided ScrollController is currently attached to more than one ScrollPosition.
          body: NestedScrollViewPlus(
              headerSliverBuilder: (context, innerBoxIsScrolled) {
                return [
                  OverlapAbsorberPlus(
                    sliver: SliverAppBar(
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
                        background: Stack(
                          fit: StackFit.expand,
                          children: [
                            DecoratedBox(
                              position: DecorationPosition.foreground,
                              decoration: BoxDecoration(
                                gradient: LinearGradient(
                                  colors: [
                                    theme.primaryColor,
                                    Colors.transparent,
                                  ],
                                  begin: Alignment.bottomCenter,
                                  end: Alignment.topCenter,
                                ),
                              ),
                              child: const Image(
                                image:
                                    AssetImage('assets/AppBarBackground.jpg'),
                                fit: BoxFit.cover,
                              ),
                            ),
                            Container(
                              padding: const EdgeInsets.all(32.0),
                              child: const Image(
                                image: AssetImage('assets/skyve_inv_logo.png'),
                                fit: BoxFit.contain,
                                opacity: AlwaysStoppedAnimation(0.3),
                              ),
                            ),
                          ],
                        ),
                      ),
                    ),
                  ),
                ];
              },
              body: SkyveFlutterForm(
                child: Form(
                  child: body,
                ),
              )),
          bottomNavigationBar: bottomNavigationBar);
    });
  }
}
