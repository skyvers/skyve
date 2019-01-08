package org.skyve.impl.generate.pwa.react;

import java.io.File;
import java.io.IOException;
import java.util.Set;
import java.util.TreeSet;

/*
Integrate font-awesome
Page listgrid
Do react native

 */
public class ReactGenerator {
	String uxui;
	String projectFolderPath;
	File srcSkyvePath;
	File srcSkyveViewsPath;
	Set<ReactComponent> components = new TreeSet<>();
	
	public ReactGenerator(String uxui, String projectFolderPath) {
		this.uxui = uxui;
		this.projectFolderPath = projectFolderPath;
		if (! new File(projectFolderPath).exists()) {
			throw new IllegalArgumentException("Prject folder " + projectFolderPath + " does not exist");
		}
		this.srcSkyvePath = new File(projectFolderPath, "src/skyve/");
		this.srcSkyveViewsPath = new File(projectFolderPath, "src/skyve/views/");
	}
	
	// copy files in the router directly over
	private void copyView() {
		
	}
	
	public void generate() throws IOException {
		srcSkyveViewsPath.mkdirs();
		new ReactRouter(this).create();
				
		for (ReactComponent component : components) {
			component.create();
		}
	}
	
	public static void main(String[] args) throws Exception {
		new ReactGenerator("desktop", "/Users/mike/Downloads/sigma-master/").generate();
	}
}
