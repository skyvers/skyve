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

		refreshFile("lib/util/skyve_rest_client.dart", "lib/util/skyve_rest_client.dart", substitutions);
		refreshFile("lib/widgets/skyve_hbox.dart", "lib/widgets/skyve_hbox.dart", substitutions);
		refreshFile("lib/widgets/skyve_menu.dart", "lib/widgets/skyve_menu.dart", substitutions);
		refreshFile("lib/widgets/skyve_vbox.dart", "lib/widgets/skyve_vbox.dart", substitutions);
		refreshFile("lib/widgets/skyve_view.dart", "lib/widgets/skyve_view.dart", substitutions);

		new FlutterRouting(this).create();

		for (FlutterView view : views) {
			view.create();
		}
	}

	public static void main(String[] args) throws Exception {
		new FlutterGenerator("desktop", "flutest", "/Users/mike/_/flutest/").generate();
	}
}
