package org.skyve.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import org.apache.commons.codec.binary.Base64;
import org.hibernate.util.SerializationHelper;

public final class StateUtil {
	/**
	 * Disallow instantiation.
	 */
	private StateUtil() {
		// no implementation
	}

	private static final String ZIP_CHARSET = "ISO-8859-1";

	public static String encode64(Serializable obj) 
	throws IOException {
		byte[] result = null;
		
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
			try (OutputStream zos = new GZIPOutputStream(baos)) {
//				ObjectOutputStream oos = new ObjectOutputStream(zos);
//				oos.writeObject(obj);
//				oos.close();
				SerializationHelper.serialize(obj, zos);
			}
			baos.flush();
			result = baos.toByteArray();
		}
		Base64 base64Codec = new Base64();

		return new String(base64Codec.encode(result), ZIP_CHARSET);
	}

	@SuppressWarnings("unchecked")
	public static <T extends Serializable> T decode64(String encoding)
	throws IOException {
		T result = null;

		Base64 base64Codec = new Base64();
		byte[] gzippedoos = base64Codec.decode(encoding.getBytes(ZIP_CHARSET));
		try (ByteArrayInputStream bais = new ByteArrayInputStream(gzippedoos)) {
			try (InputStream zis = new GZIPInputStream(bais)) {
//    		ObjectInputStream ois = new ObjectInputStream(zis);
//    		result = ois.readObject();
//    		ois.close();
				result = (T) SerializationHelper.deserialize(zis);
			}
		}

		return result;
	}
}
