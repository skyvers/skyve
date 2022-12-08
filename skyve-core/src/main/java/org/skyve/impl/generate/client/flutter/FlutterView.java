package org.skyve.impl.generate.client.flutter;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Map.Entry;

import org.skyve.impl.bind.BindUtil;

abstract class FlutterView implements Comparable<FlutterView> {
	protected FlutterGenerator generator;
	protected String moduleName;
	protected String viewName;
	protected String className;
	protected String fileName;
	
	FlutterView(FlutterGenerator generator, String moduleName, String viewName) {
		this.generator = generator;
		this.moduleName = moduleName;
		this.viewName = viewName; 
		className = BindUtil.toJavaTypeIdentifier(moduleName + viewName);
		fileName = BindUtil.toJavaStaticIdentifier(viewName).toLowerCase() + ".dart";
	}
	
	@Override
	public int compareTo(FlutterView o) {
		return className.compareTo(o.className);
	}
	
	void create() throws IOException {
		File modulePath = new File(generator.libViewsPath, moduleName);
		modulePath.mkdir();
System.out.println(modulePath);
		File component = new File(modulePath, fileName);
		if (component.exists()) {
			component.delete();
		}
System.out.println(component);

		try (FileWriter fw = new FileWriter(component)) {
			create(fw);
			fw.flush();
		}
	}
	
	protected String substitute(String resourcePath, Map<String, String> substitutions) throws IOException {
		String result = null;
		
		try (InputStream is = getClass().getResourceAsStream(resourcePath)) {
			result = new String(is.readAllBytes(), StandardCharsets.UTF_8);
		}
		
		if (substitutions != null) {
			for (Entry<String, String> substitution : substitutions.entrySet()) {
				result = result.replace(substitution.getKey(), substitution.getValue());
			}
		}
		
		return result;
	}

	protected abstract void create(FileWriter fw) throws IOException;
}
