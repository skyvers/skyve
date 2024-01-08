mixin Sizable {
  late final int? pixelWidth;
  late final int? responsiveWidth;
  late final int? sm;
  late final int? md;
  late final int? lg;
  late final int? xl;
  late final int? percentageWidth;
  late final int? minPixelWidth;
  late final int? maxPixelWidth;

  late final int? pixelHeight;
  late final int? percentageHeight;
  late final int? minPixelHeight;
  late final int? maxPixelHeight;
}

mixin Bordered {
  late final bool border;
  late final String? borderTitle;
}

mixin Invisible {
  late final String? invisibleConditionName;
}

mixin Disabled {
  late final String? disabledConditionName;
}

mixin Identifiable {
  late final String? widgetId;
}

mixin Decorated {
  final Map<String, String> properties = {};
}
