import 'dart:math';

import 'package:flutter/foundation.dart';
import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/responsive_grid.dart';
import 'package:skyve_flutter/util/skyve_interfaces.dart';
import 'package:skyve_flutter/views/skyve_responsive_view.dart';
import 'package:skyve_flutter/widgets/skyve_tabpane.dart';
import 'package:skyve_flutter/widgets/skyve_vbox.dart';
import '../widgets/loader.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../util/skyve_providers.dart';

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

class _SkyveEditViewState extends ConsumerState<SkyveEditView>
    with SkyveResponsiveView {
  final _stateKey =
      GlobalKey(debugLabel: 'SkyveEditView-LoaderWidget-globalkey');
  List<Widget> _contained = const [];
  List<Widget> _actions = const [];

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

  _build(BuildContext context, SkyveAbstractEditView view) {
    _contained = view.contained(context);
    _actions = view.actions(context);
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
    return responsiveView(
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
            constraints: BoxConstraints(
              minHeight: SkyveResponsiveView.screenSize.height,
            ),
            child: Padding(
              padding: const EdgeInsets.all(
                ResponsiveWidth.defaultPadding,
              ),
              child: Column(
                mainAxisAlignment: MainAxisAlignment.start,
                children: [
                  SkyveVBox(children: _contained),
                ],
              ),
            ),
          ),
        ),
      ),
    );
  }

  @override
  BottomAppBar? bottomNavigationBar(BuildContext context) {
    BottomAppBar? result;
    if (_actions.isNotEmpty) {
      Widget guts = SingleChildScrollView(
        scrollDirection: Axis.horizontal,
        child: Container(
          constraints: BoxConstraints(
              minWidth: SkyveResponsiveView.small
                  // NB -20 for padding
                  ? SkyveResponsiveView.screenSize.width - 20.0
                  : max(
                      // NB -menuWidth for left padding -10 for right padding
                      SkyveResponsiveView.screenSize.width -
                          SkyveResponsiveView.menuWidth -
                          10.0,
                      SkyveResponsiveView.menuWidth,
                    )),
          child: Wrap(
            alignment: WrapAlignment.center,
            spacing: 8.0,
            children: _actions,
          ),
        ),
      );
      if (kIsWeb) {
        guts = Padding(
          padding: SkyveResponsiveView.small
              ? const EdgeInsets.all(10.0)
              : const EdgeInsets.only(
                  left: SkyveResponsiveView.menuWidth,
                  right: 10.0,
                  top: 10.0,
                  bottom: 10.0),
          child: guts,
        );
      } else {
        guts = Padding(
          padding: SkyveResponsiveView.small
              ? const EdgeInsets.symmetric(horizontal: 10.0)
              : const EdgeInsets.only(
                  left: SkyveResponsiveView.menuWidth,
                  right: 10.0,
                ),
          child: guts,
        );
      }
      result = BottomAppBar(child: guts);
    }
    return result;
  }

  @override
  PreferredSizeWidget? appBarBottomContents(BuildContext context) {
    if ((_contained.length == 1) && (_contained[0] is SkyveTabPane)) {
      return const PreferredSize(
        preferredSize: Size(300, 20),
// TODO cant get the tab pane stateful widget exposed to place tabBar in here.
// Have to place it above the scaffold - see https://docs.flutter.dev/cookbook/design/tabs
        child: Text('Tab Pane'),
      );
    }

    return null;
  }
}
