defmodule Msg do

  ## The most common records types
  @a 1
  @ns 2
  @cname 5
  @soa 6
  @ptr 12
  @mx 15
  @txt 16
  @aaaa 28

  ## The only class we care about

  @int 1

  def decode(<<id::16, flags::binary-size(2), qdc::16, anc::16, nsc::16, arc::16, body::binary>> = raw) do
    <<qr::1, op::4, aa::1, tc::1, rd::1, ra::1, _::3, resp::4>> = flags
    body = decode_body(qdc, anc, nsc, arc, body, raw)
    {id, qr, op, aa, tc, rd, ra, resp, body}
  end

  def fake(<<_::16, rest::binary>>, id) do
    <<id::16, rest::binary>>
  end
  
  
  def decode_body(qdc, anc, nsc, arc, body, raw) do
    {query, rest} = decode_query(qdc, body, raw)
    {answer, rest} = decode_answer(anc, rest, raw)
    {authority, rest} = decode_answer(nsc, rest, raw)
    {additional, _} = decode_answer(arc, rest, raw)
    {query, answer, authority, additional}
  end

  def decode_query(0, body, _), do: {[], body}
  def decode_query(n, queries, raw) do
    {name, <<qtype::16, qclass::16, next::binary>>} = decode_name(queries, raw)
    {decoded, body} = decode_query(n - 1, next, raw)
    {[{name, qtype, qclass} | decoded], body}
  end

  def decode_answer(0, body, _), do: {[], body}
  def decode_answer(n, answers, raw) do
    {name, <<type::16, class::16, ttl::32, rdlength::16, rest::binary>>} = decode_name(answers, raw)
    {rdata, next} = decode_rdata(rdlength, rest)
    record = decode_record(type, class, rdata, raw)
    {decoded, body} = decode_answer(n - 1, next, raw)
    {[{name,  ttl, record} | decoded], body}
  end

  def decode_rdata(n, rest) do
    <<rdata::binary-size(n), next::binary>> = rest
    {rdata, next}
  end

  def decode_record(@a, @int, <<i1::8, i2::8, i3::8, i4::8>>, _) do
    {:a, :int, {i1, i2, i3, i4}}
  end
  def decode_record(@ns, @int, rdata, raw) do
    {name, _} = decode_name(rdata, raw)
    {:ns, :int, name}
  end
  def decode_record(@cname, @int, rdata, raw) do
    {name, _} = decode_name(rdata, raw)
    {:cname, :int, name}
  end
  def decode_record(@soa, @int, rdata, raw) do
    {primary, rd1} = decode_name(rdata, raw)
    {admin, rd2} = decode_name(rd1, raw)
    <<serial::32, refr::32, retr::32, exp::32, ttl::32>> = rd2
    {:soa, :int, primary, admin, serial, refr, retr, exp, ttl}
  end
  def decode_record(@mx, @int, <<pred::16, rdata::binary>>, raw) do
    {name, _} = decode_name(rdata, raw)
    {:mx, :int, pred, name}
  end
  def decode_record(@ptr, @int, rdata, raw) do
    {name, _} = decode_name(rdata, raw)
    {:ptr, :int, name}
  end
  def decode_record(@txt, @int, rdata, _) do
    {:txt, :int, :binary.binary_to_list(rdata)}
  end
  def decode_record(@aaaa, @int, rdata, _) do
    {:ipv6, :int, rdata}
  end
  def decode_record(type, class, rdata, _) do
    {type, class, rdata}
  end

  def decode_name(label, raw) do
    decode_names(label, [], raw)
  end

  def decode_names(<<0::1, 0::1, 0::6, rest::binary>>, names, _raw) do  
    {Enum.reverse(names), rest}
  end
  def decode_names(<<0::1, 0::1, n::6, _::binary>> = label, names, raw) do
    ## regular name encoding
    <<_::8, name::binary-size(n), rest::binary>> = label
    decode_names(rest, [name|names], raw)
  end
  def decode_names(<<1::1, 1::1, n::14, rest::binary>>, names, raw) do
    ## offset encoding
    <<_::binary-size(n), section::binary>> = raw
    {name, _} = decode_names(section, names, raw)
    {name, rest}
  end

end
