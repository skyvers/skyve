import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';

class SkyveContentSignature extends StatelessWidget with Sizable {
  final String label;

  SkyveContentSignature(
      {super.key, required this.label, int? pixelWidth, int? pixelHeight}) {
    this.pixelWidth = pixelWidth;
    this.pixelHeight = pixelHeight;
  }

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SizedBox(
        width: pixelWidth?.toDouble() ?? 400.0,
        height: pixelHeight?.toDouble() ?? 200.0,
        child: Stack(alignment: Alignment.center, children: [
          const Placeholder(fallbackHeight: 50, color: Colors.orange),
          Text('ContentLink: $label')
        ]));
  }
}
