import 'package:##PROJECT##/widgets/skyve_menu.dart';
import 'package:flutter/material.dart';

class SkyveView {
  static const int sm = 576;
  static const int md = 768;
  static const int lg = 992;
  static const int xl = 1200;

  static Widget responsiveView(BuildContext context, String currentRoute,
      String viewTitle, Widget view) {
    return LayoutBuilder(builder: (context, constraints) {
      final bool mobile = (constraints.maxWidth <= SkyveView.sm);
      final Drawer? drawer = mobile
          ? Drawer(child: SkyveMenu(currentRoute: currentRoute, inDrawer: true))
          : null;
      Widget body;
      if (mobile) {
        body = view;
      } else {
        body = Row(children: [
          SizedBox(
              width: 240,
              child: Column(children: [
                Expanded(
                    child:
                        SkyveMenu(currentRoute: currentRoute, inDrawer: false))
              ])),
          Expanded(child: view)
        ]);
      }

      return Scaffold(
          appBar: AppBar(title: Text(viewTitle)), drawer: drawer, body: body);
    });
  }
}
