import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';

class SkyveLabel extends Text with Sizable {
  SkyveLabel(String value,
      {Key? key, bool required = false, int? pixelWidth, int? pixelHeight})
      : super(value + (required ? ' *' : ''), key: key) {
    this.pixelWidth = pixelWidth;
    this.pixelHeight = pixelHeight;
  }
}
