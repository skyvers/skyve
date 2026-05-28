package modules.admin.Communication.actions;

import java.io.InputStream;

import org.apache.commons.codec.binary.Base64;
import org.skyve.domain.messages.UploadException;
import org.skyve.metadata.controller.Upload;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.util.FileUtil;
import org.skyve.web.WebContext;

import modules.admin.domain.Communication;

/**
 * Uploads an image and inserts its reference into communication content.
 */
public class AddImage extends UploadAction<Communication> {

	/**
	 * Performs the upload operation.
	 * @param communication the communication value
	 * @param upload the upload value
	 * @param exception the exception value
	 * @param webContext the webContext value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public Communication upload(Communication communication, Upload upload, UploadException exception, WebContext webContext) throws Exception {

		Communication result = communication;

		try (InputStream in = upload.getInputStream()) {
			String base64EncodedImage = Base64.encodeBase64String(FileUtil.bytes(in));
			StringBuilder imageTag = new StringBuilder();
			imageTag.append("<div><img src=\"data:image/png;base64,");
			imageTag.append(base64EncodedImage);
			imageTag.append("\" /></div>");
			
			if (result.getBody() == null) {
				result.setBody(imageTag.toString());
			} else {
				result.setBody(result.getBody() + "<p/>" + imageTag.toString());
			}
		}		
		return result;
	}
}
