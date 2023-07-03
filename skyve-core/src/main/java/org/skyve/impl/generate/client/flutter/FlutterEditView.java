package org.skyve.impl.generate.client.flutter;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.impl.generate.client.flutter.FlutterGenerator.GeneratorConfig;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;

public class FlutterEditView extends FlutterView {
    private Module module;
    private Document document;
    private View createView;
    private View editView;

    private final User user;

    public FlutterEditView(FlutterGenerator generator, String moduleName, String viewName) {
        super(generator, moduleName, viewName);

        user = new SuperUser();
        user.setCustomerName(generator.getConfig().getCustomerName());
    }

    public void setViews(Module module, Document document) {
        this.module = module;
        this.document = document;
        GeneratorConfig config = generator.getConfig();
        Customer c = config.getCustomer();
        editView = document.getView(config.getUxui(), c, ViewType.edit.toString());
        createView = document.getView(config.getUxui(), c, ViewType.create.toString());
     }

    @Override
    protected void create(FileWriter fw) throws IOException {
        Set<String> imports = new TreeSet<>();
        String editDart = null;
        // FIXME the create view may be needed at some point (possibly folded together with the edit view)
//        String createDart = null;

        String uxui = generator.getConfig().getUxui();

        if (editView != null) {
            FlutterViewRenderer v = new FlutterViewRenderer(user, module, document, editView, uxui, imports);
            v.visit();
            editDart = v.getResult().toString();
        }
//        if (createView != null) {
//            FlutterViewRenderer v = new FlutterViewRenderer(user, module, document, createView, uxui, imports);
//            v.visit();
//            createDart = v.getResult().toString();
//        }

        String projectName = generator.getConfig().getProjectName();
        Map<String, String> subs = new TreeMap<>();
        subs.put("##PROJECT##", projectName);
        subs.put("##MODULE##", moduleName);
        subs.put("##DOCUMENT##", document.getName());
        subs.put("##CLASS##", className);

        StringBuilder sb = new StringBuilder(256);
        for (String key : imports) {
            sb.append("import 'package:").append(projectName).append("/").append(key).append(".dart';\n");
        }
        subs.put("##IMPORTS##", sb.toString());
        subs.put("##DART##", editDart);

        fw.write(substitute("templates/edit.dart", subs));
    }
}
