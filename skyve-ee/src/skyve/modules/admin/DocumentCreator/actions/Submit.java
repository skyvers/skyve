package modules.admin.DocumentCreator.actions;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.MessageSeverity;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.repository.document.DocumentMetaData;
import org.skyve.impl.metadata.repository.module.ModuleMetaData;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.DocumentCreator.SkyveScriptInterpreter;
import modules.admin.domain.DocumentCreator;

public class Submit implements ServerSideAction<DocumentCreator> {

	private static final long serialVersionUID = -9077781738031503002L;

	@Override
	public ServerSideActionResult<DocumentCreator> execute(DocumentCreator bean, WebContext webContext) throws Exception {

		if (bean.getOutputLocation() == null) {
			throw new ValidationException(new Message(DocumentCreator.outputLocationPropertyName, "Output location is required"));
		}

		parseScript(bean, webContext);

		return new ServerSideActionResult<>(bean);
	}

	@SuppressWarnings("boxing")
	private static void parseScript(DocumentCreator bean, WebContext webContext) {
		// disect the script into documents, attributes, associations and collections
		// split the string into lines
		// String[] lines = bean.getScript().split("\n");

		// DocumentMetaData currentDocument = null;

		/*for (String line : lines) {
			System.out.println("current line: " + line);
			if (line.startsWith("##")) {
				if (currentDocument == null) {
					// create a new document
					currentDocument = new DocumentMetaData();
					line = line.replace("##", "").trim();
					currentDocument.setName(line);
				}
			}
		}*/

		SkyveScriptInterpreter i = new SkyveScriptInterpreter(bean.getScript());
		i.preProcess();
		i.process();

		if (i.getModules().size() > 0) {
			StringBuilder output = new StringBuilder();
			final String sourceDirectory = bean.getOutputLocation();

			for(ModuleMetaData m : i.getModules()) {
				output.append(XMLMetaData.marshalModule(m, false));
				XMLMetaData.marshalModule(m, sourceDirectory);
				final String moduleDirectory = String.format("%s/modules/%s", sourceDirectory, m.getName());
						
				if (i.getDocuments().size() > 0) {
					for (DocumentMetaData d : i.getDocuments()) {
						output.append("<br>");
						output.append(XMLMetaData.marshalDocument(d, false));
						XMLMetaData.marshalDocument(d, moduleDirectory);
					}
				}
			}

			// System.out.println(output);
			// bean.setDocumentPreview(output.toString());

			webContext.growl(MessageSeverity.info, String.format("Wrote %d modules and %d documents to %s",
					i.getModules().size(),
					i.getDocuments().size(),
					bean.getOutputLocation()));
		} else {
			webContext.growl(MessageSeverity.warn, "No modules were detected, please check your script or refer to the Help tab.");
		}
	}
}
