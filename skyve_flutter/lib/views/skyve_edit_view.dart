import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/responsive_grid.dart';
import 'package:skyve_flutter/widgets/skyve_vbox.dart';
import '../widgets/loader.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../util/skyve_providers.dart';
import '../widgets/skyve_view.dart';

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
    List<Widget> widgets = view.contained(context);
    List<Widget> actions = view.actions(context);
/*
    Widget toolbar = actions.isEmpty
        ? const SizedBox(width: 0, height: 0)
        : Container(
            padding: const EdgeInsets.all(10.0),
            child: Wrap(
              alignment: WrapAlignment.center,
              spacing: 8.0,
              children: actions,
            ),
          );
*/
    return SkyveView.responsiveView(
        context: context,
        viewTitle: ref.watch(viewStateProvider).title,
        view: LoaderWidget(
            key: _stateKey,
            module: widget.m,
            document: widget.d,
            bizId: widget.i,
            child: SingleChildScrollView(
              // make sure the container is at least as high as the screen but will be as large as the child column needs
              // this will align the column to the top of the container when its height is smaller than screen height
              child: Container(
                  alignment: Alignment.topCenter,
                  constraints:
                      BoxConstraints(minHeight: SkyveView.screenSize.height),
                  child: Padding(
                    padding:
                        const EdgeInsets.all(ResponsiveWidth.defaultPadding),
                    child: Column(
                        mainAxisAlignment: MainAxisAlignment.start,
                        children: [SkyveVBox(children: widgets)]),
                  )),
            )),
        appBarBottomContents: const PreferredSize(
            preferredSize: Size(300, 20), child: Text('skyve_edit_view')),
        actions: actions);
  }
}
