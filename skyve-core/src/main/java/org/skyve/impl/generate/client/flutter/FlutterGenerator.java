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
        refreshAll("lib/util",
                List.of("skyve_rest_client.dart", "skyve_providers.dart", "skyve_form.dart", "validators.dart"),
                substitutions);

        // lib models folder
        refreshAll("lib/models", List.of("skyve_datasource_models.dart", "skyve_menu_models.dart",
                "skyve_view_models.dart", "payload.dart"), substitutions);

        // lib widgets folder
        refreshAll("lib/widgets",
                List.of("skyve_border.dart", "skyve_button.dart", "skyve_checkbox.dart", "skyve_colourpicker.dart",
                        "skyve_combo.dart", "skyve_contentimage.dart", "skyve_datagrid.dart", "responsive_layout.dart",
                        "skyve_hbox.dart", "skyve_label.dart", "skyve_menu.dart", "skyve_network_image.dart",
                        "skyve_spacer.dart", "skyve_tab.dart", "skyve_tabpane.dart", "skyve_textfield.dart",
                        "skyve_toolbar.dart", "skyve_vbox.dart", "skyve_view.dart", "skyve_contentlink.dart",
                        "skyve_contentsignature.dart", "skyve_html.dart", "skyve_lookupdescription.dart",
                        "skyve_password.dart", "skyve_radio.dart", "skyve_richtext.dart", "skyve_spinner.dart",
                        "skyve_textarea.dart", "skyve_actionlink.dart", "skyve_report.dart", "skyve_download.dart",
                        "skyve_upload.dart", "skyve_staticimage.dart", "skyve_dynamicimage.dart", "skyve_blurb.dart",
                        "skyve_formitem.dart", "skyve_formrow.dart", "skyve_formcolumn.dart", "loader.dart"),
                substitutions);

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

        public void addModocWhitelistEntry(String modocEntry) {
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
