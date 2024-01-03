import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';

class SkyveZoomIn extends StatelessWidget with Sizable {
  SkyveZoomIn(
      {super.key,
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
    Widget result = const Stack(alignment: Alignment.center, children: [
      Placeholder(fallbackHeight: 50, color: Colors.orange),
      Text('ZoomIn')
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
