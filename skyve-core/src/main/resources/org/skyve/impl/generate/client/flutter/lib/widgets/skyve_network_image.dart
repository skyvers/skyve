import 'package:flutter/material.dart';
import 'package:flutter/widgets.dart';
import 'package:cached_network_image/cached_network_image.dart';

import '../util/skyve_rest_client.dart';

class SkvyeNetworkImage extends StatelessWidget {
  const SkvyeNetworkImage({super.key});

  final String _logoString = 'skyve-logo-black.png';

  ///  Builds the DrawerHeader image with a default cache time of 7 days
  @override
  Widget build(BuildContext context) {
    return CachedNetworkImage(
      imageUrl: logoUri,
      alignment: Alignment.center,
      fadeInDuration: const Duration(seconds: 1),
      fit: BoxFit.contain,
    );
  }

  String get logoUri {
    return '${SkyveRestClient().getBaseUri()}resources?_n=$_logoString';
  }
}
