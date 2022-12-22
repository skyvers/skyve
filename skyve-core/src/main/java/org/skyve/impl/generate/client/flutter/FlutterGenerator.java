package org.skyve.impl.generate.client.flutter;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeSet;

import org.skyve.domain.messages.DomainException;

public class FlutterGenerator {
	public static final String INDENT = "  ";
	
	String uxui;
	String projectName;
	File projectPath;
	File libPath;
	File libViewsPath;
	Set<FlutterView> views = new TreeSet<>();

	public FlutterGenerator(String uxui, String projectName, String projectPath) {
		this.uxui = uxui;
		this.projectName = projectName;
		this.projectPath = new File(projectPath);
		if (! this.projectPath.exists()) {
			throw new IllegalArgumentException("Project folder " + projectPath + " does not exist");
		}
		this.libPath = new File(projectPath, "lib/");
		this.libViewsPath = new File(projectPath, "lib/views/");
	}

	void refreshFile(String resourcePath, String flutterPath) throws IOException {
		refreshFile(resourcePath, flutterPath, null);
	}

	void refreshFile(String resourcePath, String flutterPath, Map<String, String> substitutions) throws IOException {
		File file = new File(projectPath, flutterPath);
		if (file.exists()) {
			if (! file.delete()) {
				throw new DomainException("Could not delete file " + file.getCanonicalPath());
			}
		}

		String flutterContent = null;
		try (InputStream is = getClass().getResourceAsStream(resourcePath)) {
			flutterContent = new String(is.readAllBytes(), StandardCharsets.UTF_8);
		}
		
		if (substitutions != null) {
			for (Entry<String, String> substitution : substitutions.entrySet()) {
				flutterContent = flutterContent.replace(substitution.getKey(), substitution.getValue());
			}
		}
		
		try (FileWriter fw = new FileWriter(file)) {
			fw.write(flutterContent);
		}
	}

	public void generate() throws IOException {
		libViewsPath.mkdirs();
		
		Map<String, String> substitutions = Collections.singletonMap("##PROJECT##", projectName);
		refreshFile("pubspec.yaml", "pubspec.yaml", substitutions);

		new File(projectPath, "lib/util/").mkdir();
		refreshFile("lib/util/skyve_rest_client.dart", "lib/util/skyve_rest_client.dart", substitutions);
		new File(projectPath, "lib/widgets/").mkdir();
		refreshFile("lib/widgets/skyve_border.dart", "lib/widgets/skyve_border.dart", substitutions);
		refreshFile("lib/widgets/skyve_button.dart", "lib/widgets/skyve_button.dart", substitutions);
		refreshFile("lib/widgets/skyve_checkBox.dart", "lib/widgets/skyve_checkbox.dart", substitutions);
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
		refreshFile("lib/widgets/skyve_contentsignature.dart", "lib/widgets/skyve_contentsignature.dart", substitutions);
		refreshFile("lib/widgets/skyve_html.dart", "lib/widgets/skyve_html.dart", substitutions);
		refreshFile("lib/widgets/skyve_lookupdescription.dart", "lib/widgets/skyve_lookupdescription.dart", substitutions);
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

	public static void main(String[] args) throws Exception {
		new FlutterGenerator("desktop", "flutest", "/Users/mike/_/flutest/").generate();
	}
}
