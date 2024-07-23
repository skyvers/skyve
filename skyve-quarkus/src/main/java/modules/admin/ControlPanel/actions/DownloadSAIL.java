package modules.admin.ControlPanel.actions;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.util.UUID;

import javax.tools.JavaFileObject;

import org.skyve.content.MimeType;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.util.FileUtil;
import org.skyve.util.RuntimeCompiler;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;

public class DownloadSAIL extends DownloadAction<ControlPanelExtension> {
	@Override
	public void prepare(ControlPanelExtension bean, WebContext webContext)
	throws Exception {
		ExecuteSAIL.executeSAIL(bean);
	}
	
	@Override
	public Download download(ControlPanelExtension bean, WebContext webContext)
	throws Exception {
		String sailSource = sailSource(bean.getUnescapedResults(), bean.getSailBaseUrl());
		bean.setResults(null);
		JavaFileObject java = RuntimeCompiler.javaSource("Sail", sailSource);
		
		Download result = null;
		File outputFolder = null;
		try {
			String outputFolderPath = RuntimeCompiler.COMPILE_PATH + UUID.randomUUID().toString();
			outputFolder = new File(outputFolderPath);
			outputFolder.mkdirs();
			if (RuntimeCompiler.compile(java,
											outputFolderPath,
											RuntimeCompiler.COMPILE_PATH + "junit-4.12.jar",
											RuntimeCompiler.COMPILE_PATH + "skyve-core.jar",
											RuntimeCompiler.COMPILE_PATH + "test.jar")) {
				ByteArrayOutputStream out = new ByteArrayOutputStream(2048);
				FileUtil.createJarArchive(outputFolder, out);
				return new Download("sail.jar", out.toByteArray(), MimeType.gzip);
			}
			throw new DomainException("Something went wrong. The compiler returned a false.");
		}
		catch (Exception e) {
			String msg = e.getLocalizedMessage();
			if (msg == null) {
				msg = "Something went wrong. The compiler threw a " + e.getClass();
			}
			msg = sailSource + "\n" + msg;
			result = new Download("sail.txt", msg, MimeType.plain);
		}
		finally {
			if (outputFolder != null) {
				FileUtil.delete(outputFolder);
			}
		}
		return result;
	}
	
	static String sailSource(String methods, String baseUrl) {
		StringBuilder result = new StringBuilder(2048);

		result.append("import java.io.File;\n\n");
		result.append("import org.junit.After;\n");
		result.append("import org.junit.Before;\n");
		result.append("import org.junit.Test;\n\n");
		result.append("import util.sail.BrowserConfiguration;\n");
		result.append("import util.sail.Devices;\n");
		result.append("import util.sail.PrimeFacesTest;\n\n");
		result.append("public class Sail extends PrimeFacesTest {\n");
		result.append("\t@Before\n");
		result.append("\tpublic void setup() throws Exception {\n");
		result.append("\t\tsetupChrome(new BrowserConfiguration().baseUrl(\"").append(baseUrl).append("\").pathToDriver((File.pathSeparatorChar == ':') ? \"./chromedriver\" : \"./chromedriver.exe\").userAgentString(Devices.ipad.userAgentString));\n");
		result.append("\t}\n\n");
		result.append("\t@After\n");
		result.append("\tpublic void teardown() {\n");
		result.append("\t\ttearDownBrowser();\n");
		result.append("\t}\n");
		result.append(methods);
		result.append("}\n");
		return result.toString();
	}
}
