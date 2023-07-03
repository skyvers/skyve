package org.skyve.impl.generate.client.flutter;

import static java.util.Collections.singletonMap;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.customer.Customer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FlutterGenerator {
    public static final String INDENT = "  ";
    
    private Logger log = LoggerFactory.getLogger(getClass());

    private GeneratorConfig config;
    Set<FlutterView> views = new TreeSet<>();

    private File projectPath;

    public FlutterGenerator(GeneratorConfig config) {
        this.config = config;
        Validate.notEmpty(config.getUxui());
        Validate.notEmpty(config.getProjectName());
        Validate.notEmpty(config.getProjectPath());
        Validate.notEmpty(config.getCustomerName());

        this.projectPath = new File(config.getProjectPath());
        if (!this.projectPath.exists()) {
            throw new IllegalArgumentException("Project folder " + projectPath + " does not exist");
        }
        config.setLibViewsPath(new File(projectPath, "lib/views/"));

        config.setCustomer(ProvidedRepositoryFactory.get()
                                                    .getCustomer(config.getCustomerName()));
    }

    public GeneratorConfig getConfig() {
        return config;
    }

    void refreshFile(String resourcePath, String flutterPath, Map<String, String> substitutions) throws IOException {
        File file = new File(projectPath, flutterPath);
        if (file.exists()) {
            if (!file.delete()) {
                throw new DomainException("Could not delete file " + file.getCanonicalPath());
            }
        }

        String flutterContent = null;
        try (InputStream is = getClass().getResourceAsStream(resourcePath)) {
            if (is == null) {
                throw new FlutterGeneratorException("Unable to read resource with path: '" + resourcePath + "'");
            }
            flutterContent = new String(is.readAllBytes(), StandardCharsets.UTF_8);
        }

        for (Entry<String, String> substitution : substitutions.entrySet()) {
            flutterContent = flutterContent.replace(substitution.getKey(), substitution.getValue());
        }

        log.debug(String.format("Refreshing file '%s' with substitutions: %s", file, substitutions.keySet()));

        try (FileWriter fw = new FileWriter(file)) {
            fw.write(flutterContent);
        }
    }

    /**
     * Refresh all the given files in the provided directory. Creating the
     * containing output directory if necessary.
     * 
     * @param relativeFolderPath
     * @param substitutions
     * @throws IOException
     */
    private void refreshAll(String relativeFolderPath, Collection<String> fileNames, Map<String, String> substitutions)
            throws IOException {

        File dir = new File(projectPath, relativeFolderPath);
        dir.mkdir();
        if (!dir.exists()) {
            throw new FlutterGeneratorException("Unable to creat output folder: " + dir.getAbsolutePath());
        }

        for (String fileName : fileNames) {
            String relativeFileName = relativeFolderPath + "/" + fileName;
            refreshFile(relativeFileName, relativeFileName, substitutions);
        }
    }

    public void generate() throws IOException {
        File libViewsPath = config.getLibViewsPath();
        libViewsPath.mkdirs();

        Map<String, String> substitutions = singletonMap("##PROJECT##", config.getProjectName());
        refreshFile("pubspec.yaml", "pubspec.yaml", substitutions);

        // lib views folder
        List<String> viewFiles = List.of("auto_log_in.dart", "skyve_container.dart", "skyve_edit_view.dart",
                "skyve_list_view.dart", "skyve_pluto_list_view.dart");
        refreshAll("lib/views", viewFiles, substitutions);

        // lib util folder
        refreshAll("lib/util", List.of("skyve_rest_client.dart", "skyve_providers.dart"), substitutions);

        // lib models folder
        refreshAll("lib/models",
                List.of("skyve_datasource_models.dart", "skyve_menu_models.dart", "skyve_view_models.dart"),
                substitutions);

        // lib widgets folder
        new File(projectPath, "lib/widgets/").mkdir();
        refreshFile("lib/widgets/skyve_border.dart", "lib/widgets/skyve_border.dart", substitutions);
        refreshFile("lib/widgets/skyve_button.dart", "lib/widgets/skyve_button.dart", substitutions);
        refreshFile("lib/widgets/skyve_checkbox.dart", "lib/widgets/skyve_checkbox.dart", substitutions);
        refreshFile("lib/widgets/skyve_colourpicker.dart", "lib/widgets/skyve_colourpicker.dart", substitutions);
        refreshFile("lib/widgets/skyve_combo.dart", "lib/widgets/skyve_combo.dart", substitutions);
        refreshFile("lib/widgets/skyve_contentimage.dart", "lib/widgets/skyve_contentimage.dart", substitutions);
        refreshFile("lib/widgets/skyve_datagrid.dart", "lib/widgets/skyve_datagrid.dart", substitutions);
        refreshFile("lib/widgets/skyve_form.dart", "lib/widgets/skyve_form.dart", substitutions);
        refreshFile("lib/widgets/skyve_hbox.dart", "lib/widgets/skyve_hbox.dart", substitutions);
        refreshFile("lib/widgets/skyve_label.dart", "lib/widgets/skyve_label.dart", substitutions);
        refreshFile("lib/widgets/skyve_menu.dart", "lib/widgets/skyve_menu.dart", substitutions);
        refreshFile("lib/widgets/skyve_network_image.dart", "lib/widgets/skyve_network_image.dart", substitutions);
        refreshFile("lib/widgets/skyve_spacer.dart", "lib/widgets/skyve_spacer.dart", substitutions);
        refreshFile("lib/widgets/skyve_tab.dart", "lib/widgets/skyve_tab.dart", substitutions);
        refreshFile("lib/widgets/skyve_tabpane.dart", "lib/widgets/skyve_tabpane.dart", substitutions);
        refreshFile("lib/widgets/skyve_textfield.dart", "lib/widgets/skyve_textfield.dart", substitutions);
        refreshFile("lib/widgets/skyve_toolbar.dart", "lib/widgets/skyve_toolbar.dart", substitutions);
        refreshFile("lib/widgets/skyve_vbox.dart", "lib/widgets/skyve_vbox.dart", substitutions);
        refreshFile("lib/widgets/skyve_view.dart", "lib/widgets/skyve_view.dart", substitutions);
        refreshFile("lib/widgets/skyve_contentlink.dart", "lib/widgets/skyve_contentlink.dart", substitutions);
        refreshFile("lib/widgets/skyve_contentsignature.dart", "lib/widgets/skyve_contentsignature.dart",
                substitutions);
        refreshFile("lib/widgets/skyve_html.dart", "lib/widgets/skyve_html.dart", substitutions);
        refreshFile("lib/widgets/skyve_lookupdescription.dart", "lib/widgets/skyve_lookupdescription.dart",
                substitutions);
        refreshFile("lib/widgets/skyve_password.dart", "lib/widgets/skyve_password.dart", substitutions);
        refreshFile("lib/widgets/skyve_radio.dart", "lib/widgets/skyve_radio.dart", substitutions);
        refreshFile("lib/widgets/skyve_richtext.dart", "lib/widgets/skyve_richtext.dart", substitutions);
        refreshFile("lib/widgets/skyve_spinner.dart", "lib/widgets/skyve_spinner.dart", substitutions);
        refreshFile("lib/widgets/skyve_textarea.dart", "lib/widgets/skyve_textarea.dart", substitutions);
        refreshFile("lib/widgets/skyve_actionlink.dart", "lib/widgets/skyve_actionlink.dart", substitutions);
        refreshFile("lib/widgets/skyve_report.dart", "lib/widgets/skyve_report.dart", substitutions);
        refreshFile("lib/widgets/skyve_download.dart", "lib/widgets/skyve_download.dart", substitutions);
        refreshFile("lib/widgets/skyve_upload.dart", "lib/widgets/skyve_upload.dart", substitutions);
        refreshFile("lib/widgets/skyve_staticimage.dart", "lib/widgets/skyve_staticimage.dart", substitutions);
        refreshFile("lib/widgets/skyve_dynamicimage.dart", "lib/widgets/skyve_dynamicimage.dart", substitutions);
        refreshFile("lib/widgets/skyve_blurb.dart", "lib/widgets/skyve_blurb.dart", substitutions);
        refreshFile("lib/widgets/skyve_formitem.dart", "lib/widgets/skyve_formitem.dart", substitutions);
        refreshFile("lib/widgets/skyve_formrow.dart", "lib/widgets/skyve_formrow.dart", substitutions);
        refreshFile("lib/widgets/skyve_formcolumn.dart", "lib/widgets/skyve_formcolumn.dart", substitutions);

        new FlutterRouting(this).create();

        for (FlutterView view : views) {
            view.create();
        }
    }

    public static class GeneratorConfig {

        private String uxui;
        private String projectName;
        private String projectPath;
        private File libViewsPath;
        private String customerName;
        private Collection<MoDoc> modocWhitelist = new ArrayList<>();
        private Customer customer;

        public String getUxui() {
            return uxui;
        }

        public void setCustomer(Customer customer) {
            this.customer = customer;
        }

        public Customer getCustomer() {
            return customer;
        }

        public void setUxui(String uxui) {
            this.uxui = uxui;
        }

        public String getProjectName() {
            return projectName;
        }

        public void setProjectName(String projectName) {
            this.projectName = projectName;
        }

        public String getProjectPath() {
            return projectPath;
        }

        public void setProjectPath(String projectPath) {
            this.projectPath = projectPath;
        }

        public String getCustomerName() {
            return customerName;
        }

        public void setCustomerName(String customerName) {
            this.customerName = customerName;
        }

        public Collection<MoDoc> getModocWhitelist() {
            return modocWhitelist;
        }

        public void addModocWhitelistEnty(String modocEntry) {
            modocWhitelist.add(MoDoc.fromString(modocEntry));
        }

        public File getLibViewsPath() {
            return libViewsPath;
        }

        public void setLibViewsPath(File libViewsPath) {
            this.libViewsPath = libViewsPath;
        }

        public boolean allowsMoDoc(String module, String document) {

            return modocWhitelist.stream()
                                 .anyMatch(md -> md.matches(module, document));
        }
    }

    public static class MoDoc {

        public static final String WILDCARD = "*";

        private final String module;
        private final String document;

        public MoDoc(String module, String document) {
            this.module = Validate.notEmpty(module);
            this.document = Validate.notEmpty(document);
        }

        public static MoDoc fromString(String modocString) {

            String module = StringUtils.substringBefore(modocString, ".");
            String document = StringUtils.substringAfter(modocString, ".");

            return new MoDoc(module, document);
        }

        public String getModule() {
            return module;
        }

        public String getDocument() {
            return document;
        }

        public boolean matches(String mod, String doc) {

            return (this.module.equals(WILDCARD) || this.module.equals(mod))
                    && (this.document.equals(WILDCARD) || this.document.equals(doc));
        }

        @Override
        public String toString() {
            return "MoDoc[" + module + "." + document + "]";
        }
    }
}
