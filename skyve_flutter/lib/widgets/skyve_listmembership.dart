import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';

class SkyveListMembership extends StatelessWidget with Sizable {
  final String candidatesHeading;
  final String membersHeading;

  SkyveListMembership(
      {super.key,
      required this.candidatesHeading,
      required this.membersHeading,
      int? pixelWidth,
      int? minPixelHeight}) {
    // Sizable
    this.pixelWidth = pixelWidth;
    this.minPixelHeight = minPixelHeight;
  }

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    Widget result = const Stack(alignment: Alignment.center, children: [
      Placeholder(fallbackHeight: 200, color: Colors.orange),
      Text('ListMemebership')
    ]);
    if (pixelWidth != null) {
      result = SizedBox(width: pixelWidth!.toDouble(), child: result);
    }
    return result;
  }
}
