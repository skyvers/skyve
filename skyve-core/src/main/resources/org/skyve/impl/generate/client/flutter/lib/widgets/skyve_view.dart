import 'package:flutter/material.dart';
import '../util/skyve_form.dart';
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
  List<Widget> actions(BuildContext context);

  /// Produces the widgets for the view.
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

      return Scaffold(
        appBar: AppBar(title: Text(viewTitle)),
        drawer: drawer,
        body: SkyveForm(
          child: Form(
            child: body,
          ),
        ),
      );
    });
  }
}
