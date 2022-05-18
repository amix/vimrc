require 'spec_helper'

describe 'EctoEnum' do
  i <<~EOF
  defmodule Onemedical.Types do
    import EctoEnum
    defenum(Work.Occupation, :work_occupation, [
      :actor, :architect, :athlete, :baker, :bank_clerk, :banker, :barber, :blogger,
      :bricklayer, :broadcaster, :builder, :captain, :carpenter, :choreographer,
      :computer_engineer, :computer_programmer, :custom_officer, :dancer, :designer,
      :director, :doctor, :driver, :editor, :entertainer, :engineer, :facility_manager,
      :farmer, :fashion_designer, :geologist, :goldsmith, :graphic_designer, :hairdresser,
      :host_hostess, :house_girl, :interior_designer, :judge, :land_surveyor, :lecturer,
      :make_up_artist, :manager, :mechanic, :midwife, :model, :music_director, :musician,
      :nanny, :nurse, :pastor, :paediatrician, :photographer, :physicist, :pilot, :plumber,
      :police_officer, :printer, :producer, :publisher, :quality_inspector, :radiographer,
      :real_estate_agent, :referee, :refuse_collector, :registrar, :safety_engineer, :sales_manager,
      :script_writer, :secretary, :security_guard, :shoemaker, :songwriter, :sound_engineer,
      :stock_broker, :surveyor, :tailor, :teacher, :telecommunications_engineer, :usher,
      :waiter, :writer, :zookeeper, :other])
    defenum(Work.Type, :work_type, [
      :full_time, :part_time, :volunteer, :temporary
    ])
  end
  EOF
end
