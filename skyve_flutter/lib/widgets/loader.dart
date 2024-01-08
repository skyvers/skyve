import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:skyve_flutter/util/skyve_providers.dart';

import '../models/payload.dart';
import '../util/skyve_flutter_form.dart';
import '../util/skyve_rest_client.dart';

/// A simple proxy widget, will initiate a load for the
/// document once with the given details after the widget
/// context is ready; then render the child once the
/// server response is received
///
/// You'll likely need to provide a GlobalKey when
/// constructing an instance of this widget
class LoaderWidget extends ConsumerStatefulWidget {
  final Widget child;
  final String module;
  final String document;
  final String? bizId;

  /// The key is mandatory here as re-parenting the loader
  /// widget (as happens in SkyveView.reresponsiveView) would
  /// otherwise reset the state
  const LoaderWidget({
    required super.key,
    required this.child,
    required this.module,
    required this.document,
    required this.bizId,
  });

  @override
  ConsumerState<ConsumerStatefulWidget> createState() => _LoaderWidgetState();
}

class _LoaderWidgetState extends ConsumerState<LoaderWidget> {
  // Once _ready has been set to true this widget should
  // just passthrough to the supplied widget.child
  // It shouldn't be possible for this widget to reset
  // itself to _ready=false;
  bool _ready = false;

  @override
  Widget build(BuildContext context) {
    // TODO the intention is to provide a place to put
    // error handling & display, and retries

    return Visibility(
        visible: _ready,
        replacement: const Center(child: CircularProgressIndicator()),
        child: widget.child);
  }

  void _loadData() async {
    Payload payload = await SkyveRestClient()
        .edit(widget.module, widget.document, widget.bizId);

    if (payload.successful) {
      setState(() {
        SkyveFlutterForm.of(context).applyPayload(payload);
        ref.read(viewStateProvider.notifier).title(payload.title);
        _ready = true;
      });
    } else {
      // TODO error handling
      debugPrint('TODO Something went wrong while loading');
    }
  }

  @override
  void didChangeDependencies() {
    super.didChangeDependencies();

    if (!_ready) {
      _loadData();
    }
  }
}
