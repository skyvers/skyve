import 'package:flutter/material.dart';

abstract class SkyveAbstractEditView {
  // A tree of Widgets to render in the body
  List<Widget> contained(BuildContext context);
  // A list of actions to place in the action bar.
  List<Widget> actions(BuildContext context);
}
