import 'package:flutter/material.dart';
import 'package:flutter_gen_test/util/skyve_form.dart';
import 'package:flutter_gen_test/util/skyve_rest_client.dart';

class SkyveContentImage extends StatelessWidget {
  final String label;
  final String propertyKey;

  const SkyveContentImage({
    super.key,
    required this.label,
    required this.propertyKey,
  });

  @override
  Widget build(BuildContext context) {
    var formState = SkyveForm.of(context);
    String? contentId = formState.beanValues[propertyKey];

    if (contentId != null) {
      var imageUrl = SkyveRestClient().contentUrl(
          module: formState.moduleName,
          document: formState.documentName,
          binding: propertyKey,
          contentId: contentId);

      return Image.network(imageUrl);
    } else {
      return const Placeholder(
        color: Colors.grey,
        fallbackHeight: 50,
      );
    }
  }
}