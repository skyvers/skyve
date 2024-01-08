import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';

class SkyveSlider extends StatelessWidget with Sizable {
  final String label;

  SkyveSlider(
      {super.key,
      required this.label,
      int? pixelWidth,
      int? pixelHeight,
      int? minPixelHeight,
      int? maxPixelHeight}) {
    this.pixelWidth = pixelWidth;
    this.pixelHeight = pixelHeight;
    this.minPixelHeight = minPixelHeight;
    this.maxPixelHeight = maxPixelHeight;
  }

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    Widget result = Stack(alignment: Alignment.center, children: [
      const Placeholder(fallbackHeight: 50, color: Colors.orange),
      Text('Slider: $label')
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
