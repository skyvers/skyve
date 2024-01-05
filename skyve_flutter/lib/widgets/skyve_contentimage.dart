import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';
import '../util/skyve_flutter_form.dart';
import '../util/skyve_rest_client.dart';

class SkyveContentImage extends StatelessWidget with Sizable {
  final String label;
  final String propertyKey;

  SkyveContentImage(
      {super.key,
      required this.label,
      required this.propertyKey,
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
    var formState = SkyveFlutterForm.of(context);
    String? contentId = formState.beanValues[propertyKey];

    if (contentId != null) {
      var imageUrl = SkyveRestClient.contentUrl(
          module: formState.moduleName,
          document: formState.documentName,
          binding: propertyKey,
          contentId: contentId);

      return Image.network(imageUrl,
          // TODO size to pixels based on responsive etc
          width: pixelWidth?.toDouble(),
          height: pixelHeight?.toDouble());
    } else {
      return Container(
          decoration: BoxDecoration(border: Border.all(color: Colors.grey)),
          width: pixelWidth?.toDouble(),
          height: pixelHeight?.toDouble());
    }
  }
}
