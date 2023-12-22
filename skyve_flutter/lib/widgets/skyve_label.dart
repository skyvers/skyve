import 'package:flutter/material.dart';

class SkyveLabel extends Text {
  const SkyveLabel(String value, {Key? key, bool required = false})
      : super(value + (required ? ' *' : ''), key: key);
}
