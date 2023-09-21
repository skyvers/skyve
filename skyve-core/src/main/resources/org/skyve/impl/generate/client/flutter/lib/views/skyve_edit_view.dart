import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/bean_container.dart';
import '../util/skyve_providers.dart';
import '../util/skyve_rest_client.dart';
import '../widgets/skyve_button.dart';
import '../widgets/skyve_view.dart';
import '../widgets/skyve_toolbar.dart';

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
    implements BeanContainerState {
  BeanContainer _container = BeanContainer.loading();

  @override
  set container(BeanContainer container) {
    setState(() {
      _container = container;
    });
  }

  @override
  BeanContainer get container => _container;

  get _bean {
    return container.values;
  }

  @override
  void initState() {
    super.initState();
    _load(widget.m, widget.d, widget.i);
  }

  @override
  Widget build(BuildContext context) {
    return ref.watch(containerViewProvider('${widget.m}.${widget.d}')).when(
        loading: () => const Center(child: CircularProgressIndicator()),
        error: (err, stack) => Center(child: Text('Error: $err')),
        data: (view) => _build(context, view));
  }

  _build(BuildContext context, SkyveView view) {
    List<Widget> widgets = List.from(view.contained(context, this));
    widgets.insert(
        0,
        SkyveToolbar(children: [
          Container(
              padding: const EdgeInsets.symmetric(horizontal: 10.0),
              child: Wrap(
                  alignment: WrapAlignment.center,
                  spacing: 8.0,
                  children: view.actions(context, this)))
        ]));

    return SkyveView.responsiveView(
        context,
        _bean['_title'],
        Visibility(
            visible: (_bean['bizId'] != null),
            replacement: const Center(child: CircularProgressIndicator()),
            child: SingleChildScrollView(child: Column(children: widgets))));
  }

  void _load(String m, String d, String? i) async {
    if (_bean['bizId'] == null) {
      container = await SkyveRestClient().edit(m, d, i);



    }
  }
}
