import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import '../models/skyve_datasource_models.dart';
import '../util/skyve_providers.dart';
import '../util/skyve_rest_client.dart';
import '../widgets/skyve_view.dart';

class SkyveListView extends ConsumerStatefulWidget {
  final String m;
  final String? d;
  final String q;

  const SkyveListView({Key? key, required this.m, this.d, required this.q})
      : super(key: key);

  @override
  ConsumerState<ConsumerStatefulWidget> createState() {
    return _SkyveListViewState();
  }
}

class _SkyveListViewState extends ConsumerState<SkyveListView> {
  final SkyveRestClient _rest = SkyveRestClient();
  late String _dataSourceName;
  late SkyveDataSourceModel _dataSource;
  bool _loaded = false;
  List<dynamic> _rows = [];

  @override
  void initState() {
    super.initState();

    _dataSourceName = _rest.dataSource(widget.m, widget.d, widget.q);
    ref.read(containerDataSourceProvider).whenData((dss) {
      _dataSource = dss[_dataSourceName]!;
      _fetch(0, 75);
    });
  }

  @override
  Widget build(BuildContext context) {
    const delete = SizedBox(
        width: 45.0,
        height: 70.0,
        child:
            Center(child: Icon(Icons.delete, color: Colors.white, size: 40.0)));

    return SkyveView.responsiveView(
        context,
        'Cant get title here', // TODO _dataSource['title']!,
        Visibility(
            visible: _loaded,
            replacement: const Center(child: CircularProgressIndicator()),
            child: Stack(children: [
              ListView.builder(
                  itemCount: _rows.length,
                  itemBuilder: (context, index) {
                    return Dismissible(
                        key: Key(_rows[index]['bizId']),
                        background: Container(
                            color: Colors.red,
                            child: Wrap(
                                alignment: WrapAlignment.spaceBetween,
                                crossAxisAlignment: WrapCrossAlignment.center,
                                children: const [delete, delete])),
                        onDismissed: (direction) {
                          ScaffoldMessenger.of(context).showSnackBar(
                              const SnackBar(
                                  content: SizedBox(
                                      height: 50.0,
                                      child: Center(child: Text('Deleted')))));
                        },
                        child: Card(
                            child: ListTile(
                                title: Text(nvl(
                                    _rows[index][_dataSource.fields[0].name])),
                                subtitle: (_dataSource.fields.length == 1)
                                    ? null
                                    : Text(nvl(_rows[index]
                                        [_dataSource.fields[1].name])),
                                trailing: const Icon(Icons.chevron_right),
                                onTap: () {
                                  context
                                      .push(Uri(path: '/e', queryParameters: {
                                    'm': _rows[index]['bizModule'],
                                    'd': _rows[index]['bizDocument'],
                                    'i': _rows[index]['bizId']
                                  }).toString());
                                })));
                  }),
              Positioned(
                bottom: 40.0,
                right: 40.0,
                child: FloatingActionButton(
                    child: const Icon(Icons.add),
                    onPressed: () {
                      context.push(Uri(path: '/e', queryParameters: {
                        'm': _dataSource.module,
                        'd': _dataSource.document
                      }).toString());
                    }),
              )
            ])));
  }

  void _fetch(int startRow, int endRow) async {
    final rows = await _rest.fetchDataSource(_dataSourceName, startRow, endRow);
    setState(() {
      _rows = rows;
      _loaded = true;
    });
  }
}
