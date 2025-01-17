import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';

class SkyveGeometryMap extends StatelessWidget with Sizable {
  final String label;

  SkyveGeometryMap(
      {super.key,
      required this.label,
      int? pixelWidth,
      int? responsiveWidth,
      int? percentageWidth,
      int? sm,
      int? md,
      int? lg,
      int? xl,
      int? minPixelWidth,
      int? maxPixelWidth,
      int? pixelHeight,
      int? percentageHeight,
      int? minPixelHeight,
      int? maxPixelHeight}) {
    // Sizable
    this.pixelWidth = pixelWidth;
    this.responsiveWidth = responsiveWidth;
    this.percentageWidth = percentageWidth;
    this.sm = sm;
    this.md = md;
    this.lg = lg;
    this.xl = xl;
    this.minPixelWidth = minPixelWidth;
    this.maxPixelWidth = maxPixelWidth;

    this.pixelHeight = pixelHeight;
    this.percentageHeight = percentageHeight;
    this.minPixelHeight = minPixelHeight;
    this.maxPixelHeight = maxPixelHeight;
  }

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return Stack(alignment: Alignment.center, children: [
      const Placeholder(fallbackHeight: 200, color: Colors.orange),
      Text('GeometryMap: $label')
    ]);
  }
}
