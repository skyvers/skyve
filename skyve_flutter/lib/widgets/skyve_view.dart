import 'package:flutter/material.dart';
import 'package:nested_scroll_view_plus/nested_scroll_view_plus.dart';
import '../util/skyve_flutter_form.dart';
import 'skyve_menu.dart';

String nvl(dynamic value) {
  return (value == null) ? '' : value.toString();
}

abstract class SkyveView {
  static const int sm = 576;
  static const int md = 768;
  static const int lg = 992;
  static const int xl = 1200;

  // Produces the widgets for the action bar.
  List<Widget> actions(BuildContext context);

  // Produces the widgets for the view.
  List<Widget> contained(BuildContext context);

  static Widget responsiveView(
      BuildContext context, String viewTitle, Widget view) {
    return LayoutBuilder(builder: (context, constraints) {
      final bool mobile = (constraints.maxWidth <= SkyveView.sm);
      final Drawer? drawer =
          mobile ? const Drawer(child: SkyveMenu(inDrawer: true)) : null;
      Widget body;
      if (mobile) {
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
          body: NestedScrollViewPlus(
              headerSliverBuilder: (context, innerBoxIsScrolled) {
                return [
                  SliverAppBar(
                    pinned: true,
                    snap: true,
                    floating: true,
                    stretch: true,
                    expandedHeight: mobile ? 120.0 : 160.0,
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
              )));
    });
  }
}
