import 'package:##PROJECT##/util/skyve_rest_client.dart';
import 'package:##PROJECT##/widgets/skyve_view.dart';
import 'package:##PROJECT##/widgets/skyve_button.dart';
import 'package:##PROJECT##/models/bean_container.dart';
import 'package:flutter/material.dart';
import 'package:flutter_bootstrap/flutter_bootstrap.dart';
##IMPORTS##

class ##CLASS## extends StatefulWidget {
  static const routeName = '/##MODULE##/##DOCUMENT##';

  final Map<String,String> queryParams;

  const ##CLASS##({Key? key, this.queryParams = const {}}) : super(key: key);

  @override
  State<StatefulWidget> createState() {
    return _##CLASS##State();
  }
  
  String? get _bizId {
    return queryParams['bizId'];
  }
}

class _##CLASS##State extends State<##CLASS##>
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

  Map<String, dynamic> get _bean => _container.values;
  
  void _load(String? bizId) async {
    if (_bean['bizId'] == null) {
      container =
          await SkyveRestClient().edit('##MODULE##', '##DOCUMENT##', bizId);
    }
  }

  @override
  Widget build(BuildContext context) {
    final bizId = widget._bizId;
    _load(bizId);
    return SkyveView.responsiveView(
        context,
        _bean['_title'],
        Visibility(
            visible: (_bean['bizId'] != null),
            replacement: const Center(child: CircularProgressIndicator()),
            child: SingleChildScrollView(
              child: ##DART##
            )
        )
    );
  }



}
