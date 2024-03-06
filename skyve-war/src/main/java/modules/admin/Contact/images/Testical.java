package modules.admin.Contact.images;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;

import org.skyve.metadata.model.document.DynamicImage;
import org.skyve.metadata.user.User;

import modules.admin.domain.Contact;

public class Testical implements DynamicImage<Contact> {
	
	@Override
	public BufferedImage getImage(Contact bean, int width, int height, User user) throws Exception {
		BufferedImage result = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
		Graphics2D g = (Graphics2D) result.getGraphics();
		g.setColor(Color.BLACK);
		g.setStroke(new BasicStroke(50F));
		g.drawRect(0, 0, width, height);
		return result;
	}

	@Override
	public ImageFormat getFormat() {
		// don't care
		return null;
	}

	@Override
	public Float getCompressionQuality() {
		// don't care
		return null;
	}
}