package modules.admin.DocumentCreator.actions;

import org.commonmark.node.Node;
import org.commonmark.renderer.NodeRenderer;
import org.commonmark.renderer.html.HtmlNodeRendererContext;
import org.commonmark.renderer.html.HtmlNodeRendererFactory;
import org.commonmark.renderer.html.HtmlRenderer;
import org.skyve.impl.script.SkyveScriptInterpreter;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.DocumentCreator.SkyveDocumentNodeRenderer;
import modules.admin.domain.DocumentCreator;

public class UpdatePreview implements ServerSideAction<DocumentCreator> {

	private static final long serialVersionUID = -682204740567953097L;

	@Override
	public ServerSideActionResult<DocumentCreator> execute(DocumentCreator bean, WebContext webContext) throws Exception {
		String script = bean.getScript();
		if (script == null) {
			bean.setMarkdownPreview(null);
			bean.setDocumentPreview(null);
		}
		else {
			SkyveScriptInterpreter i = new SkyveScriptInterpreter(bean.getScript());
			i.preProcess();
			Node document = i.parse();

			// Parser parser = Parser.builder().build();
			// Node document = parser.parse(script);
	
			// create a markdown to HTML renderer for the markdown preview tab
			HtmlRenderer htmlRenderer = HtmlRenderer.builder().build();
	
			// create a markdown to document XML renderer for the document preview tab
			HtmlRenderer documentRenderer = HtmlRenderer.builder().nodeRendererFactory(new HtmlNodeRendererFactory() {
				@Override
				public NodeRenderer create(HtmlNodeRendererContext context) {
					return new SkyveDocumentNodeRenderer(context, true);
				}
			}).build();
	
			// update the previews
			bean.setMarkdownPreview(htmlRenderer.render(document));
			bean.setDocumentPreview(documentRenderer.render(document));
		}
		
		return new ServerSideActionResult<>(bean);
	}

}
