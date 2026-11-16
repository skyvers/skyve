package modules.utmod.UtDoc;

import org.skyve.util.test.SkyveFactory;

@SkyveFactory(excludedUpdateAttributes = { "auditStamp", "lockVersion" })
public class UtDocFactory {
	// Marker class used for annotation-driven test discovery.
	public UtDocFactory() {
		// nothing to initialise: the class only carries its @SkyveFactory annotation
	}
}
