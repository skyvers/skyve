import 'package:flutter/material.dart';

class SkyveBorder extends Container {
  SkyveBorder({Key? key, String? title, Widget? child})
      : super(
            key: key,
            child: child,
            decoration: BoxDecoration(border: Border.all()),
            margin: const EdgeInsets.all(8.0));
}
