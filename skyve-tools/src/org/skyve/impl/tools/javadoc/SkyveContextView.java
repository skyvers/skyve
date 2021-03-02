package org.skyve.impl.tools.javadoc;

import java.io.IOException;

//import org.umlgraph.doclet.AccessibleOptions;
//import org.umlgraph.doclet.ContextView;
//import org.umlgraph.doclet.Options;

//import com.sun.javadoc.ClassDoc;
//import com.sun.javadoc.RootDoc;

//DC JAVA 11
//public class SkyveContextView extends ContextView {
public class SkyveContextView {
//	private ClassDoc classDoc;
//
//	public SkyveContextView(String outputFolder,
//								ClassDoc cd,
//								RootDoc root,
//								Options parent)
//	throws IOException {
//		super(outputFolder, cd, root, parent);
//		classDoc = cd;
//	}
//
//    @Override
//	public Options getOptionsFor(ClassDoc cd) {
//    	Options result = super.getOptionsFor(cd);
//    	if (cd.equals(this.classDoc)) {
//    		overrideCentreClass(result);
//    	}
//    	new AccessibleOptions(result).setEdgeColor("dimgray");
//
//    	return result;
//    }
//
//    @Override
//	public Options getOptionsFor(String name) {
//    	Options result = super.getOptionsFor(name);
//    	if (name.equals(this.classDoc.name())) {
//    		overrideCentreClass(result);
//    	}
//    	new AccessibleOptions(result).setEdgeColor("dimgray");
//
//    	return result;
//    }
//
//	@Override
//	public void overrideForClass(Options opt, ClassDoc cd) {
//		super.overrideForClass(opt, cd);
//		if (cd.equals(this.classDoc)) {
//			overrideCentreClass(opt);
//		}
//    	new AccessibleOptions(opt).setEdgeColor("dimgray");
//	}
//
//	@Override
//	public void overrideForClass(Options opt, String className) {
//		super.overrideForClass(opt, className);
//		if (className.equals(this.classDoc.name())) {
//			overrideCentreClass(opt);
//		}
//    	new AccessibleOptions(opt).setEdgeColor("dimgray");
//	}
//
//	private static void overrideCentreClass(Options opt) {
//		AccessibleOptions aopt = new AccessibleOptions(opt);
//		aopt.setShowAttributes(true);
//		aopt.setShowEnumConstants(true);
//		aopt.setShowEnumerations(true);
//		aopt.setShowType(true);
//	}
}
