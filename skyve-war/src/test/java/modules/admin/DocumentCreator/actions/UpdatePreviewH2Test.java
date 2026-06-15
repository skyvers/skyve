package modules.admin.DocumentCreator.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.domain.DocumentCreator;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class UpdatePreviewH2Test extends AbstractH2Test {
	@Test
	void executeWithNullScriptClearsPreviewsAndReturnsBean() throws Exception {
		DocumentCreator bean = DocumentCreator.newInstance();
		bean.setMarkdownPreview("old markdown");
		bean.setDocumentPreview("old document");
		bean.setScript(null);

		ServerSideActionResult<DocumentCreator> result = new UpdatePreview().execute(bean, null);

		assertThat(result.getBean(), is(bean));
		assertNull(bean.getMarkdownPreview());
		assertNull(bean.getDocumentPreview());
	}
}
