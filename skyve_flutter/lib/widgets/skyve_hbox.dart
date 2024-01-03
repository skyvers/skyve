import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/responsive_grid.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';
import 'package:skyve_flutter/widgets/skyve_border.dart';

class SkyveHBox extends StatelessWidget with Sizable, Bordered {
  final List<Widget> children;

  SkyveHBox(
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
    List<ResponsiveItem> items = List.empty(growable: true);

    List<ResponsiveWidth> itemWidths = _computeWidths();
    int index = 0;
    for (Widget child in children) {
      if (child is Sizable) {
        ResponsiveWidth itemWidth = itemWidths[index];
        // padding applied to all items but the first
        EdgeInsets? padding;
        if (index > 0) {
          padding = itemWidth.willBreak()
              ? const EdgeInsets.only(top: ResponsiveWidth.defaultPadding)
              : const EdgeInsets.only(left: ResponsiveWidth.defaultPadding);
        }
        final ResponsiveItem item = ResponsiveItem(
            width: itemWidth,
            padding: padding,
            child: child);
        items.add(item);
        index++;
      }
    }
    var result = ResponsiveGrid(children: [ResponsiveRow(children: items)]);
    if (border) {
      return SkyveBorder(title: borderTitle, child: result);
    }
    return result;
  }

  List<ResponsiveWidth> _computeWidths() {
    int unsizedCols = 0;
    int mediumColsRemaining = ResponsiveWidth.maxResponsiveWidthColumns;

    List<ResponsiveWidth> result = List.empty(growable: true);
    for (Widget child in children) {
      if (child is Sizable) {
        Sizable sizable = child as Sizable;
        if (sizable.pixelWidth != null) {
          mediumColsRemaining -=
              ResponsiveWidth.pixelWidthToMediumResponsiveWidth(
                  sizable.pixelWidth!.toDouble());
        } else if (sizable.percentageWidth != null) {
          mediumColsRemaining -=
              ResponsiveWidth.percentageWidthToResponsiveWidth(
                  sizable.percentageWidth!.toDouble());
        } else {
          unsizedCols++;
        }
      }
    }
    int? percentageWidth = ((unsizedCols == 0) || (mediumColsRemaining < 1))
        ? null
        : ResponsiveWidth.responsiveWidthToPercentageWidth(
            mediumColsRemaining / unsizedCols);

    for (Widget child in children) {
      if (child is Sizable) {
        Sizable sizable = child as Sizable;

        int small = ResponsiveWidth.maxResponsiveWidthColumns;
        int medium = ResponsiveWidth.maxResponsiveWidthColumns;
        int large = ResponsiveWidth.maxResponsiveWidthColumns;
        int extraLarge = ResponsiveWidth.maxResponsiveWidthColumns;

        if (sizable.responsiveWidth != null) {
          medium = sizable.responsiveWidth!;
          large = medium;
          extraLarge = medium;
        } else if (sizable.pixelWidth != null) {
          medium = ResponsiveWidth.pixelWidthToMediumResponsiveWidth(
              sizable.pixelWidth!.toDouble());
          large = ResponsiveWidth.pixelWidthToLargeResponsiveWidth(
              sizable.pixelWidth!.toDouble());
          extraLarge = large;
        } else if (percentageWidth != null) {
          medium = ResponsiveWidth.percentageWidthToResponsiveWidth(
              percentageWidth.toDouble());
          large = medium;
          extraLarge = medium;
        }

        if (sizable.sm != null) {
          small = sizable.sm!;
        }
        if (sizable.md != null) {
          medium = sizable.md!;
        }
        if (sizable.lg != null) {
          large = sizable.lg!;
        }
        if (sizable.xl != null) {
          extraLarge = sizable.xl!;
        }

        result.add(ResponsiveWidth(small, medium, large, extraLarge));
      }
    }

    return result;
  }
}
