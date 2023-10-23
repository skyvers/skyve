import 'package:flutter/material.dart';
import 'package:flutter_gen_test/widgets/loader.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../util/skyve_providers.dart';
import '../widgets/skyve_view.dart';
import '../widgets/skyve_toolbar.dart';

/// A dynamic edit view which reacts to changes in the containerViewProvider
class SkyveEditView extends ConsumerStatefulWidget {
  final String m;
  final String d;
  final String? i;

  const SkyveEditView({Key? key, required this.m, required this.d, this.i})
      : super(key: key);

  @override
  ConsumerState<ConsumerStatefulWidget> createState() {
    return _SkyveEditViewState();
  }
}

class _SkyveEditViewState extends ConsumerState<SkyveEditView> {
  final _stateKey =
      GlobalKey(debugLabel: 'SkyveEditView-LoaderWidget-globalkey');

  @override
  Widget build(BuildContext context) {
    return ref
        .watch(containerViewProvider('${widget.m}.${widget.d}')) //
        .when(
          loading: () => const Center(child: CircularProgressIndicator()),
          error: (err, stack) => Center(child: Text('Error: $err')),
          data: (view) => _build(context, view),
        );
  }

  _build(BuildContext context, SkyveView view) {
    List<Widget> widgets = List.from(view.contained(context));
    widgets.insert(
        0,
        SkyveToolbar(children: [
          Container(
              padding: const EdgeInsets.symmetric(horizontal: 10.0),
              child: Wrap(
                  alignment: WrapAlignment.center,
                  spacing: 8.0,
                  children: view.actions(context)))
        ]));

    return SkyveView.responsiveView(
        context,
        'FIXME title',
        LoaderWidget(
          key: _stateKey,
          module: widget.m,
          document: widget.d,
          bizId: widget.i,
          child: SingleChildScrollView(child: Column(children: widgets)),
        ));
  }
}
