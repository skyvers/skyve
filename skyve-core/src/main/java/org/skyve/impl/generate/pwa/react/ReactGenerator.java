package org.skyve.impl.generate.pwa.react;

import java.io.File;
import java.io.IOException;
import java.util.Set;
import java.util.TreeSet;

import org.skyve.impl.metadata.repository.AbstractRepository;
import org.skyve.impl.metadata.repository.LocalDesignRepository;
import org.skyve.metadata.customer.Customer;

/*
Integrate font-awesome
Page listgrid
Do react native

 */
public class ReactGenerator {
	Customer customer;
	String uxui;
	AbstractRepository repository;
	String projectFolderPath;
	File srcSkyvePath;
	File srcSkyveViewsPath;
	Set<ReactComponent> components = new TreeSet<>();
	
	public ReactGenerator(String customerName, String uxui, String projectFolderPath) {
		repository = AbstractRepository.get();
		this.customer = repository.getCustomer(customerName);
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
		AbstractRepository repository = new LocalDesignRepository();
		AbstractRepository.set(repository);
		new ReactGenerator("demo", "desktop", "/Users/mike/Downloads/sigma-master/").generate();
	}
}
