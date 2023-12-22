import 'package:flutter/material.dart';
import 'package:flutter/widgets.dart';
import 'package:cached_network_image/cached_network_image.dart';
import '../util/skyve_rest_client.dart';

class SkvyeNetworkImage extends StatelessWidget {
  final String resourceName;
  final BoxFit? fit;
  final double? width;
  final double? height;

  const SkvyeNetworkImage(this.resourceName,
      {Key? key, this.fit, this.width, this.height})
      : super(key: key);

  //  Builds an Image with a default cache time of 7 days
  @override
  Widget build(BuildContext context) {
    return CachedNetworkImage(
        imageUrl: logoUri,
        alignment: Alignment.center,
        fadeInDuration: const Duration(seconds: 1),
        fit: fit ?? BoxFit.contain,
        width: width,
        height: height);
  }

  String get logoUri {
    return '${SkyveRestClient().getBaseUri()}resources?_n=$resourceName';
  }
}
