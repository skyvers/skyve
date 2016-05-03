package org.skyve.impl.tools.javadoc;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;

import org.umlgraph.doclet.AccessibleOptions;
import org.umlgraph.doclet.ContextView;
import org.umlgraph.doclet.OptionProvider;
import org.umlgraph.doclet.Options;
import org.umlgraph.doclet.UmlGraph;
import org.umlgraph.doclet.WrappedRootDoc;

import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.LanguageVersion;
import com.sun.javadoc.PackageDoc;
import com.sun.javadoc.RootDoc;
import com.sun.tools.doclets.standard.Standard;

/**
 * Chaining doclet that runs the standart Javadoc doclet first, and on success,
 * runs the generation of dot files by UMLGraph
 */
public class SkyveDoclet {
	/**
	 * Option check, forwards options to the standard doclet, if that one
	 * refuses them, they are sent to UmlGraph
	 */
	public static int optionLength(String option) {
		int result = Standard.optionLength(option);
		if (result != 0) {
			return result;
		}
		
		return UmlGraph.optionLength(option);
	}

	/**
	 * Standard doclet entry point
	 * 
	 * @param root
	 * @return
	 */
	public static boolean start(RootDoc root) {
		root.printNotice("Skyve Doclet, running the standard doclet");
		Standard.start(root);
		root.printNotice("Skyve doclet, altering javadocs");
		try {
			String outputFolder = findOutputPath(root.options());

			Options opt = UmlGraph.buildOptions(root);
			opt.setOptions(root.options());
			AccessibleOptions aopt = new AccessibleOptions(opt);
			
			// in javadoc enumerations are always printed
			aopt.setShowEnumerations(true);
			aopt.setRelativeLinksForSourcePackages(true);
			// enable strict matching for hide expressions
			aopt.setStrictMatching(true);
			// root.printNotice(opt.toString());

			RootDoc wrapped = new WrappedRootDoc(root);
			generatePackageDiagrams(wrapped, aopt, outputFolder);
			generateContextDiagrams(wrapped, aopt, outputFolder);
		} catch (Throwable t) {
			root.printWarning("Error!");
			root.printWarning(t.toString());
			t.printStackTrace();
			return false;
		}
		return true;
	}

	/**
	 * Standand doclet entry
	 * 
	 * @return
	 */
	public static LanguageVersion languageVersion() {
		return Standard.languageVersion();
	}

	/**
	 * Generates the package diagrams for all of the packages that contain
	 * classes among those returned by RootDoc.class()
	 */
	private static void generatePackageDiagrams(RootDoc root, AccessibleOptions opt,
			String outputFolder) throws IOException {
		Set<String> packages = new HashSet<>();
		for (ClassDoc classDoc : root.classes()) {
			PackageDoc packageDoc = classDoc.containingPackage();
			String packageDocName = packageDoc.name();
			AccessibleOptions newOpt = opt;
			
			if (!packages.contains(packageDocName)) {
				packages.add(packageDoc.name());
				OptionProvider view = new SkyvePackageView(outputFolder, packageDoc,
						root, newOpt.getOptions());
				UmlGraph.buildGraph(root, view, packageDoc);
				runGraphviz(newOpt.getDotExecutable(), outputFolder, packageDoc.name(),
						packageDoc.name(), root);
				alterHtmlDocs(newOpt, outputFolder, packageDoc.name(),
						packageDoc.name(), "package-summary.html",
						Pattern.compile(".*</[Hh]2>.*"), root);
			}
		}
	}

	/**
	 * Generates the context diagram for a single class
	 */
	private static void generateContextDiagrams(RootDoc root, AccessibleOptions opt,
			String outputFolder) throws IOException {
		Set<ClassDoc> classDocs = new TreeSet<>(
				new Comparator<ClassDoc>() {
					@Override
					public int compare(ClassDoc cd1, ClassDoc cd2) {
						return cd1.name().compareTo(cd2.name());
					}
				});
		for (ClassDoc classDoc : root.classes()) {
			classDocs.add(classDoc);
		}
		
		ContextView view = null;
		for (ClassDoc classDoc : classDocs) {
			AccessibleOptions newOpt = opt;
			view = new SkyveContextView(outputFolder, classDoc, root, newOpt.getOptions());

			UmlGraph.buildGraph(root, view, classDoc);
			runGraphviz(newOpt.getDotExecutable(),
							outputFolder,
							classDoc.containingPackage().name(),
							classDoc.name(),
							root);
			alterHtmlDocs(newOpt,
							outputFolder,
							classDoc.containingPackage().name(),
							classDoc.name(),
							classDoc.name() + ".html",
							Pattern.compile(".*(Class|Interface|Enum) " + classDoc.name() + ".*"),
							root);
		}
	}

	/**
	 * Runs Graphviz dot building both a diagram (in png format) and a client
	 * side map for it.
	 */
	private static void runGraphviz(String dotExecutable, String outputFolder,
			String packageName, String name, RootDoc root) {
		String modifiedDotExecutable = dotExecutable;
		if (dotExecutable == null) {
			modifiedDotExecutable = "dot";
		}
		File dotFile = new File(outputFolder, packageName.replace(".", "/")
				+ "/" + name + ".dot");
		File pngFile = new File(outputFolder, packageName.replace(".", "/")
				+ "/" + name + ".png");
		File mapFile = new File(outputFolder, packageName.replace(".", "/")
				+ "/" + name + ".map");

		try {
			Process p = Runtime.getRuntime().exec(
					new String[] { modifiedDotExecutable, "-Tcmapx", "-o",
							mapFile.getAbsolutePath(), "-Tpng", "-o",
							pngFile.getAbsolutePath(),
							dotFile.getAbsolutePath() });
			BufferedReader reader = new BufferedReader(new InputStreamReader(
					p.getErrorStream()));
			String line = null;
			while ((line = reader.readLine()) != null)
				root.printWarning(line);
			int result = p.waitFor();
			if (result != 0)
				root.printWarning("Errors running Graphviz on " + dotFile);
		} catch (Exception e) {
			e.printStackTrace();
			System.err
					.println("Ensure that dot is in your path and that its path does not contain spaces");
		}
	}

	// Format string for the uml image div tag.
	private static final String UML_DIV_TAG = "<div align=\"center\">"
			+ "<img src=\"%1$s.png\" alt=\"Package class diagram package %1$s\" usemap=\"#G\" border=0/>"
			+ "</div>";

	// Format string for the java script tag.
	private static final String EXPANDABLE_UML = "<script type=\"text/javascript\">\n"
			+ "function show() {\n"
			+ "    document.getElementById(\"uml\").innerHTML = \n"
			+ "        \'<a style=\"font-family:monospace\" href=\"javascript:hide()\">%3$s</a>\' +\n"
			+ "        \'%1$s\';\n"
			+ "}\n"
			+ "function hide() {\n"
			+ "	document.getElementById(\"uml\").innerHTML = \n"
			+ "	\'<a style=\"font-family:monospace\" href=\"javascript:show()\">%2$s</a>\' ;\n"
			+ "}\n"
			+ "</script>\n"
			+ "<div id=\"uml\" >\n"
			+ "	<a href=\"javascript:show()\">\n"
			+ "	<a style=\"font-family:monospace\" href=\"javascript:show()\">%2$s</a> \n"
			+ "</div>";

	/**
	 * Takes an HTML file, looks for the first instance of the specified
	 * insertion point, and inserts the diagram image reference and a client
	 * side map in that point.
	 */
	private static void alterHtmlDocs(AccessibleOptions opt, String outputFolder,
			String packageName, String className, String htmlFileName,
			Pattern insertPointPattern, RootDoc root) throws IOException {
		
		// setup files
		File output = new File(outputFolder, packageName.replace(".", "/"));
		File htmlFile = new File(output, htmlFileName);
		File alteredFile = new File(htmlFile.getAbsolutePath() + ".uml");
		File mapFile = new File(output, className + ".map");
		if (!htmlFile.exists()) {
			System.err.println("Expected file not found: "
					+ htmlFile.getAbsolutePath());
			return;
		}

		// parse & rewrite
		boolean matched = false;
		try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(htmlFile), 
																				opt.getOutputEncoding()));
				BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(alteredFile),
																					opt.getOutputEncoding()))) {
			String line;
			while ((line = reader.readLine()) != null) {
				writer.write(line);
				writer.newLine();
				if (!matched && insertPointPattern.matcher(line).matches()) {
					matched = true;
					if (mapFile.exists())
						insertClientSideMap(mapFile, writer);
					else
						root.printWarning("Could not find map file " + mapFile);

					String tag = String.format(UML_DIV_TAG, className);
					if (opt.isCollapsibleDiagrams())
						tag = String.format(EXPANDABLE_UML, tag,
								"Show UML class diagram",
								"Hide UML class diagram");
					writer.write("<!-- UML diagram added by Skyve doclet -->");
					writer.newLine();
					writer.write(tag);
					writer.newLine();
				}
			}
		}

		// if altered, delete old file and rename new one to the old file name
		if (matched) {
			htmlFile.delete();
			alteredFile.renameTo(htmlFile);
		} else {
			root.printNotice("Warning, could not find a line that matches the pattern '"
					+ insertPointPattern.pattern()
					+ "'.\n Class diagram reference not inserted");
			alteredFile.delete();
		}
	}

	/**
	 * Reads the map file and outputs in to the specified writer
	 * 
	 * @throws IOException
	 */
	private static void insertClientSideMap(File mapFile, BufferedWriter writer)
	throws IOException {
		try (BufferedReader reader = new BufferedReader(new FileReader(mapFile))) {
			String line = null;
			while ((line = reader.readLine()) != null) {
				writer.write(line);
				writer.newLine();
			}
		}
	}

	/**
	 * Returns the output path specified on the javadoc options
	 */
	private static String findOutputPath(String[][] options) {
		for (int i = 0; i < options.length; i++) {
			if (options[i][0].equals("-d"))
				return options[i][1];
		}
		return ".";
	}
}
