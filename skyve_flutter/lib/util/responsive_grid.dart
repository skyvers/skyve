import 'dart:math';

import 'package:flutter/material.dart';
import 'package:skyve_flutter/widgets/skyve_view.dart';

class ResponsiveWidth {
  static const int maxResponsiveWidthColumns = 12;
  static const int maxSmallScreenWidthPixels = 640;
  static const int maxMediumScreenWidthPixels = 1024;
  static const int maxLargeScreenWidthPixels = 1440;
  static const double defaultPadding = 12.0;

  int sm;
  int md;
  int lg;
  int xl;

  ResponsiveWidth(this.sm, this.md, this.lg, this.xl);

  static ResponsiveWidth of(int value) {
    return ResponsiveWidth(12, value, value, value);
  }

  // Immutable addition (for colspan calcs).
  ResponsiveWidth add(ResponsiveWidth other) {
    int small = min(sm + other.sm, 12);
    int medium = min(md + other.md, 12);
    int large = min(lg + other.lg, 12);
    int extraLarge = min(xl + other.xl, 12);
    return ResponsiveWidth(small, medium, large, extraLarge);
  }

  // Depending on the responsive break point, is the column 12?
  bool willBreak() {
    double screenWidth = SkyveView.screenSize.width;
    return ((screenWidth > ResponsiveWidth.maxLargeScreenWidthPixels) &&
            (xl == 12)) ||
        ((screenWidth > ResponsiveWidth.maxMediumScreenWidthPixels) &&
            (lg == 12)) ||
        ((screenWidth > ResponsiveWidth.maxSmallScreenWidthPixels) &&
            (md == 12)) ||
        ((screenWidth <= ResponsiveWidth.maxSmallScreenWidthPixels) &&
            (sm == 12));
  }

  static int responsiveWidthToPercentageWidth(double responsiveWidth) {
    int result = (responsiveWidth / maxResponsiveWidthColumns * 100.0).ceil();
    return min(result, 100);
  }

  static int percentageWidthToResponsiveWidth(double percentageWidth) {
    int result = (percentageWidth / 100.0 * maxResponsiveWidthColumns).ceil();
    return min(result, maxResponsiveWidthColumns);
  }

  static int pixelWidthToMediumResponsiveWidth(double pixelWidth) {
    int result =
        (pixelWidth / maxMediumScreenWidthPixels * maxResponsiveWidthColumns)
            .ceil();
    return min(result, maxResponsiveWidthColumns);
  }

  static int pixelWidthToLargeResponsiveWidth(double pixelWidth) {
    int result =
        (pixelWidth / maxLargeScreenWidthPixels * maxResponsiveWidthColumns)
            .ceil();
    return min(result, maxResponsiveWidthColumns);
  }
}

class ResponsiveGrid extends StatelessWidget {
  final BoxDecoration? decoration;
  final EdgeInsets? padding;
  final List<Widget> children;

  const ResponsiveGrid({
    Key? key,
    required this.children,
    this.decoration,
    this.padding,
  }) : super(key: key);

  @override
  Widget build(BuildContext context) {
    return LayoutBuilder(
      builder: (BuildContext context, BoxConstraints constraints) {
        Widget result = Container(
          width: constraints.maxWidth,
          decoration: decoration,
          child: Wrap(
            alignment: WrapAlignment.start,
            crossAxisAlignment: WrapCrossAlignment.start,
            direction: Axis.horizontal,
            children: children,
          ),
        );

        if (padding != null) {
          result = Padding(
            padding: padding!,
            child: result,
          );
        }

        return Align(
          alignment: Alignment.topCenter,
          child: result,
        );
      },
    );
  }
}

class ResponsiveRow extends StatelessWidget {
  // minimum row height
  final double? height;
  final BoxDecoration? decoration;
  final EdgeInsets? padding;
  final List<ResponsiveItem> children;

  const ResponsiveRow(
      {super.key,
      required this.children,
      this.decoration,
      this.padding,
      this.height});

  @override
  Widget build(BuildContext context) {
    return LayoutBuilder(
        builder: (BuildContext context, BoxConstraints constraints) {
      Widget result = Wrap(
          alignment: WrapAlignment.start,
          direction: Axis.horizontal,
          children: children);

      if (padding != null) {
        result = Padding(padding: padding!, child: result);
      }

      result = Container(
          constraints: BoxConstraints(
              minHeight: height ?? 0.0,
              minWidth: constraints.maxWidth,
              maxWidth: constraints.maxWidth),
          decoration: decoration,
          child: result);

      return result;
    });
  }
}

class ResponsiveItem extends StatelessWidget {
  static const double _oneColumnRatio =
      1 / ResponsiveWidth.maxResponsiveWidthColumns;

  // How a flexible child is inscribed into the available space.
  // If [flex] is non-zero, the [fit] determines whether the child fills the
  // space the parent makes available during layout. If the fit is
  // [FlexFit.tight], the child is required to fill the available space. If the
  // fit is [FlexFit.loose], the child can be at most as large as the available
  // space (but is allowed to be smaller).
  final FlexFit fit;

  final Widget child;
  final EdgeInsets? padding;

  // The responsive width declaration
  final ResponsiveWidth width;

  const ResponsiveItem(
      {super.key,
      required this.child,
      this.fit = FlexFit.loose,
      this.padding,
      required this.width});

  @override
  Widget build(BuildContext context) {
    return LayoutBuilder(
      builder: (BuildContext context, BoxConstraints constraints) {
        // Get the prefix for the definition, based on screen width
        double screenWidth = SkyveView.screenSize.width;
        int cols = 0;
        if (screenWidth > ResponsiveWidth.maxLargeScreenWidthPixels) {
          cols = width.xl;
        } else if (screenWidth > ResponsiveWidth.maxMediumScreenWidthPixels) {
          cols = width.lg;
        } else if (screenWidth > ResponsiveWidth.maxSmallScreenWidthPixels) {
          cols = width.md;
        } else {
          cols = width.sm;
        }

        Widget result = child;
        if (padding != null) {
          result = Padding(padding: padding!, child: result);
        }
        result = SizedBox(
            width: cols * constraints.maxWidth * _oneColumnRatio,
            child: result);

        return result;
      },
    );
  }
}
