import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';

class SkyveTextArea extends StatelessWidget with Sizable {
  final String label;

  SkyveTextArea(
      {super.key,
      required this.label,
      int? pixelWidth,
      int? pixelHeight,
      int? minPixelHeight}) {
    this.pixelWidth = pixelWidth;
    this.pixelHeight = pixelHeight;
    this.minPixelHeight = minPixelHeight;
  }

  @override
  Widget build(BuildContext context) {
    Widget result = Stack(alignment: Alignment.center, children: [
      const Placeholder(fallbackHeight: 100, color: Colors.orange),
      Text('TextArea $label')
    ]);
    if ((pixelWidth != null) || (pixelHeight != null)) {
      result = SizedBox(
          width: pixelWidth?.toDouble(),
          height: pixelHeight?.toDouble(),
          child: result);
    }
    return result;
  }
}
