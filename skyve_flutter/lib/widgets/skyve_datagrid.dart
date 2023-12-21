import 'package:flutter/material.dart';

class SkyveDataGrid extends Column {
  SkyveDataGrid(
      {Key? key,
      List<dynamic>? rows = const [],
      List<Widget>? children = const []})
      : super(key: key, children: children ?? const []);
}
