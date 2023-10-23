import 'package:flutter/material.dart';
import 'package:go_router/go_router.dart';

import '../models/bean_container.dart';
import '../util/skyve_rest_client.dart';
import '../util/skyve_form.dart';

typedef BeanSupplier = BeanContainer Function();

class SkyveButton extends StatelessWidget {
  static const List<String> _poppingActions = ['OK', 'Remove'];

  final String actionType;
  final String actionName;
  final String label;

  const SkyveButton(
      {Key? key,
      required this.actionType,
      required this.actionName,
      required this.label})
      : super(key: key);

  @override
  Widget build(BuildContext context) {
    if (actionType == 'Cancel') {
      return ElevatedButton(onPressed: () => _pop(context), child: Text(label));
    } else {
      return ElevatedButton(
          onPressed: () => _onPressed(context), child: Text(label));
    }
  }

  void _pop(BuildContext context) {
    final GoRouter router = GoRouter.of(context);
    if (router.canPop()) {
      router.pop();
    } else {
      debugPrint("WARN: Couldn't pop...");
    }
  }

  void _onPressed(BuildContext context) async {
    SkyveFormState formState = SkyveForm.of(context);
    debugPrint('Action $actionType pressed for $formState');

    // FIXME need to convert from formState to a BeanContainer
    BeanContainer bc = BeanContainer.loading();

    // Call rest client to peform the update
    BeanContainer result = await SkyveRestClient().addOrUpdate(actionName, bc);

    // FIXME and convert back...

    // If server result was successful, and this is an
    // action we should pop from we might pop here
    bool shouldPop = result.successful && _poppingActions.contains(actionName);
    if (shouldPop) {
      if (context.mounted) {
        _pop(context);
      } else {
        debugPrint('context was no longer mounted when we wanted to pop...');
        // raise error?
      }
    } else {
      // view.accept(result)
      // SkyveForm.of(context);
      // state.container = result;
      debugPrint('TODO do something with the result: $result');
    }
  }
}
