import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/responsive_grid.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';
import 'package:skyve_flutter/widgets/skyve_border.dart';

class SkyveVBox extends StatelessWidget with Sizable, Bordered {
  final List<Widget> children;

  SkyveVBox(
      {Key? key,
      required this.children,
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
      int? maxPixelHeight,
      bool? border,
      String? borderTitle})
      : super(key: key) {
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

    // Bordered
    this.border = border ?? false;
    this.borderTitle = borderTitle;
  }

  @override
  Widget build(BuildContext context) {
    List<ResponsiveRow> rows = List.empty(growable: true);
    int index = 0;
    for (Widget child in children) {
      List<ResponsiveItem> items = List.empty(growable: true);
      ResponsiveItem item =
          ResponsiveItem(width: ResponsiveWidth.of(12), child: child);
      items.add(item);
      ResponsiveRow row = ResponsiveRow(
          // padding applied to all but the first
          padding: (index == 0)
              ? null
              : const EdgeInsets.only(top: ResponsiveWidth.defaultPadding),
          children: items);
      rows.add(row);
      index++;
    }
    var result = ResponsiveGrid(children: rows);
    if (border) {
      return SkyveBorder(title: borderTitle, child: result);
    }
    return result;
  }
}
