import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';

class SkyveBlurb extends StatelessWidget with Sizable {
  final String label;

  SkyveBlurb(
      {super.key, required this.label, int? pixelWidth, int? pixelHeight}) {
    this.pixelWidth = pixelWidth;
    this.pixelHeight = pixelHeight;
  }

  @override
  Widget build(BuildContext context) {
    Widget result = const Stack(alignment: Alignment.center, children: [
      Placeholder(fallbackHeight: 50, color: Colors.orange),
      Text('Blurb')
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
