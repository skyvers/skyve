package org.skyve.wildcat.tools.javadoc;

import org.umlgraph.doclet.AccessibleOptions;
import org.umlgraph.doclet.OptionProvider;
import org.umlgraph.doclet.Options;
import org.umlgraph.doclet.PackageView;

import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.PackageDoc;
import com.sun.javadoc.RootDoc;

public class WildcatPackageView extends PackageView {

	public WildcatPackageView(String outputFolder,
								PackageDoc pd,
								RootDoc root,
								OptionProvider parent) {
		super(outputFolder, pd, root, parent);
	}

	@Override
	public void overrideForClass(Options opt, ClassDoc cd) {
		if (cd.isEnum()) {
			new AccessibleOptions(opt).hide();
		}
		else {
			super.overrideForClass(opt, cd);
		}
		new AccessibleOptions(opt).setEdgeColor("dimgray");
	}

	@Override
	public void overrideForClass(Options opt, String className) {
		super.overrideForClass(opt, className);
		new AccessibleOptions(opt).setEdgeColor("dimgray");
	}

	@Override
	public Options getOptionsFor(ClassDoc cd) {
		Options result = super.getOptionsFor(cd);		
		if (cd.isEnum()) {
			new AccessibleOptions(result).hide();
		}
		new AccessibleOptions(result).setEdgeColor("dimgray");
		return result;
	}
	
	@Override
	public Options getOptionsFor(String name) {
		Options result = super.getOptionsFor(name);
		new AccessibleOptions(result).setEdgeColor("dimgray");
		return result;
	}
}
