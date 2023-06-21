package org.skyve.impl.generate.client.react;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

abstract class ReactComponent implements Comparable<ReactComponent> {
	protected ReactGenerator generator;
	private String key;
	protected String moduleName;
	protected String componentName;
	
	ReactComponent(ReactGenerator generator, String moduleName, String componentName) {
		this.generator = generator;
		this.moduleName = moduleName;
		this.componentName = componentName; 
		key = String.format("%s.%s", moduleName, componentName);
	}
	
	@Override
	public int compareTo(ReactComponent o) {
		return key.compareTo(o.key);
	}
	
	void create() throws IOException {
		File modulePath = new File(generator.srcSkyveViewsPath, moduleName);
		modulePath.mkdir();
System.out.println(modulePath);
		File component = new File(modulePath, componentName + ".js");
		if (component.exists()) {
			component.delete();
		}
System.out.println(component);

		try (FileWriter fw = new FileWriter(component)) {
			create(fw);
			fw.flush();
		}
	}
	
	protected abstract void create(FileWriter fw) throws IOException;}

