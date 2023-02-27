import 'package:flutter/material.dart';
import 'skyve_menu.dart';

String nvl(dynamic value) {
  return (value == null) ? '' : value.toString();
}

abstract class SkyveView {
  static const int sm = 576;
  static const int md = 768;
  static const int lg = 992;
  static const int xl = 1200;

  /// Produces the widgets for the action bar.
  List<Widget> actions(BuildContext context, Map<String, dynamic> bean);

  /// Produces the widgets for the view.
  List<Widget> contained(BuildContext context, Map<String, dynamic> bean);

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
          SizedBox(
              width: 240,
              child: Column(children: const [
                Expanded(child: SkyveMenu(inDrawer: false))
              ])),
          Expanded(child: view)
        ]);
      }

      return Scaffold(
          appBar: AppBar(title: Text(viewTitle)), drawer: drawer, body: body);
    });
  }
}
