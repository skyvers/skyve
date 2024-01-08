import 'package:skyve_flutter/util/skyve_mixins.dart';

class SkyveFormColumn with Sizable {
  SkyveFormColumn(
      {int? pixelWidth,
      int? responsiveWidth,
      int? percentageWidth,
      int? sm,
      int? md,
      int? lg,
      int? xl}) {
    this.pixelWidth = pixelWidth;
    this.responsiveWidth = responsiveWidth;
    this.percentageWidth = percentageWidth;
    this.sm = sm;
    this.md = md;
    this.lg = lg;
    this.xl = xl;
  }
}
