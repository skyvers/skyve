import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';

class SkyveContentLink extends StatelessWidget with Sizable {
  final String label;

  SkyveContentLink({super.key, required this.label, int? pixelWidth}) {
    this.pixelWidth = pixelWidth;
  }

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return Stack(alignment: Alignment.center, children: [
      const Placeholder(fallbackHeight: 50, color: Colors.orange),
      Text('ContentLink: $label')
    ]);
  }
}
