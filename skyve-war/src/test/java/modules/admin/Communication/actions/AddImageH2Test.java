package modules.admin.Communication.actions;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

import org.junit.jupiter.api.Test;
import org.skyve.content.MimeType;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.WebFileInputStream;

import modules.admin.domain.Communication;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class AddImageH2Test extends AbstractH2Test {
	@Test
	void uploadSetsBodyWhenBodyIsEmpty() throws Exception {
		Communication communication = Communication.newInstance();

		Communication result = new AddImage().upload(communication, upload("image-bytes"), new UploadException(), null);

		assertThat(result, is(communication));
		assertThat(result.getBody(), containsString("<div><img src=\"data:image/png;base64,"));
		assertThat(result.getBody(), containsString("aW1hZ2UtYnl0ZXM="));
	}

	@Test
	void uploadAppendsImageWhenBodyAlreadyExists() throws Exception {
		Communication communication = Communication.newInstance();
		communication.setBody("<p>Existing</p>");

		new AddImage().upload(communication, upload("more-bytes"), new UploadException(), null);

		assertThat(communication.getBody(), containsString("<p>Existing</p><p/><div><img"));
		assertThat(communication.getBody(), containsString("bW9yZS1ieXRlcw=="));
	}

	@SuppressWarnings("resource")
	private static Upload upload(String content) {
		return new Upload("image.png",
				new WebFileInputStream(new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8))),
				MimeType.png);
	}
}
