import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../util/skyve_providers.dart';
import '../views/skyve_list_view.dart';

class SkyveContainer extends ConsumerWidget {
  const SkyveContainer({Key? key}) : super(key: key);

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final metadata = ref.watch(containerMenuProvider);
    return metadata.when(
        loading: () => const Center(child: CircularProgressIndicator()),
        error: (err, stack) => Center(child: Text('Error: $err')),
        data: (metadata) => const SkyveListView(m: 'admin', q: 'qContacts'));
  }
}
