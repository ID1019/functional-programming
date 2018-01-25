defmodule Msg do

  ## The most common records
  @a 1
  @ns 2
  @cname 5
  @soa 6
  @ptr 12
  @mx 15
  @txt 16
  @aaaa 28

  def decode(<<id::16, flags::binary-size(2), qdc::16, anc::16, nsc::16, arc::16, body::binary>> = raw) do
    <<qr::1, op::4, aa::1, tc::1, rd::1, ra::1, _::3, resp::4>> = flags
    decoded = decode_body(qdc, anc, nsc, arc, body, raw)
    {id, qr, op, aa, tc, rd, ra, resp, decoded}
  end

  def decode_body(qdc, anc, nsc, arc, body0, raw) do
    {query, body1} = decode_query(qdc, body0, raw)
    {answer, body2} = decode_answer(anc, body1, raw)
    {authority, body3} = decode_answer(nsc, body2, raw)
    {additional, _} = decode_answer(arc, body3, raw)
    {query, answer, authority, additional}
  end

  defp decode_query(0, body, _), do: {[], body}
  defp decode_query(n, queries, raw) do
    {name, <<qtype::16, qclass::16, next::binary>>} = decode_name(queries, raw)
    {decoded, body} = decode_query(n - 1, next, raw)
    {[{name, qtype, qclass} | decoded], body}
  end

  defp decode_answer(0, body, _), do: {[], body}
  defp decode_answer(n, answers, raw) do
    {name, <<type::16, class::16, ttl::32, rdlength::16, rest::binary>>} =
      decode_name(answers, raw)

    {rdata, next} = decode_rdata(rdlength, rest)
    record = decode_record(type, class, rdata, raw)
    {decoded, body} = decode_answer(n - 1, next, raw)
    {[{name, ttl, record} | decoded], body}
  end

  defp decode_rdata(n, rest) do
    <<rdata::binary-size(n), next::binary>> = rest
    {rdata, next}
  end

  defp decode_record(@a, _, <<i1::8, i2::8, i3::8, i4::8>>, _) do
    {:a, {i1, i2, i3, i4}}
  end
  defp decode_record(@ns, _, rdata, raw) do
    {name, _} = decode_name(rdata, raw)
    {:ns, name}
  end
  defp decode_record(@cname, _, rdata, raw) do
    {name, _} = decode_name(rdata, raw)
    {:cname, name}
  end
  defp decode_record(@soa, _, rdata, raw) do
    {primary, rd1} = decode_name(rdata, raw)
    {admin, rd2} = decode_name(rd1, raw)
    <<serial::32, refr::32, retr::32, exp::32, ttl::32>> = rd2
    {:soa, primary, admin, serial, refr, retr, exp, ttl}
  end
  defp decode_record(@mx, _, <<pred::16, rdata::binary>>, raw) do
    {name, _} = decode_name(rdata, raw)
    {:mx, pred, name}
  end
  defp decode_record(@ptr, _, rdata, raw) do
    {name, _} = decode_name(rdata, raw)
    {:ptr, name}
  end
  defp decode_record(@txt, _, rdata, _) do
    {:txt, :binary.binary_to_list(rdata)}
  end
  defp decode_record(@aaaa, _, rdata, _) do
    {:ipv6, rdata}
  end
  defp decode_record(type, class, rdata, _) do
    {type, class, rdata}
  end

  def decode_name(label, raw), do: decode_name(label, [], raw)
  def decode_name(<<0::1, 0::1, _::6, _::binary>> = label, names, raw) do
    ## regular name encoding
    decode_label(label, names, raw)
  end
  def decode_name(<<1::1, 1::1, n::14, rest::binary>>, names, raw) do
    ## offset encoding
    offset = 8 * n
    <<_::size(offset), section::binary>> = raw
    {name, _} = decode_label(section, names, raw)
    {name, rest}
  end

  defp decode_label(<<0::8, rest::binary>>, names, _) do
    {Enum.reverse(names), rest}
  end
  defp decode_label(<<n::8, rest::binary>>, names, raw) do
    decode_label(n, rest, [], names, raw)
  end
  defp decode_label(0, rest, sofar, names, raw) do
    decode_name(rest, [Enum.reverse(sofar) | names], raw)
  end
  defp decode_label(n, <<char::8, rest::binary>>, sofar, names, raw) do
    decode_label(n - 1, rest, [char | sofar], names, raw)
  end

end
