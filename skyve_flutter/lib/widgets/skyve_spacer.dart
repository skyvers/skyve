import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';

class SkyveSpacer extends SizedBox with Sizable {
  SkyveSpacer({Key? key, int? pixelWidth, int? pixelHeight})
      : super(
            key: key,
            width: (pixelWidth != null) ? pixelWidth.toDouble() : 1.0,
            height: (pixelHeight != null) ? pixelHeight.toDouble() : 1.0) {
    this.pixelWidth = pixelWidth;
    this.pixelHeight = pixelHeight;
  }
}
