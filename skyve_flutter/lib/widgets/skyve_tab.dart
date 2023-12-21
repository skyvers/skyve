import 'package:flutter/material.dart';
import 'package:skyve_flutter/widgets/skyve_vbox.dart';

class SkyveTab extends SkyveVBox {
  final String title;
  final String? icon;

  const SkyveTab(
      {Key? key,
      required this.title,
      this.icon,
      List<Widget> children = const []})
      : super(key: key, children: children);
}
